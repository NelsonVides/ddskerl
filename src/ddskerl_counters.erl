-module(ddskerl_counters).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.
?MODULEDOC("""
DDSketch implementation in Erlang.

This implements an optimised pre-allocated bounded bucket count, that is, on a degenerate case,
memory consumption won't grow but quantiles might lose accuracy as values escape bounds.

In this optimisation, buckets are preallocated using `m:counters`.
Values within `(0,1)` will be inserted into the underflow bucket;
values above the expected limit will be inserted into the overflow bucket.

> #### About rounding errors {: .warning}
> Due to the nature of the implementation, this algorithm will `round/1` floats when keeping track
> of the minimum and maximums, hence these values might incurr rounding of a single digit errors.
>
> All other quantiles, as well as integers, will be tracked with the precision the sketch was
> configured with.

## Calculating the bucket count
If you have an expected highest value `M` and a given error `E`,
your ideal bucket count would be given by the formula:
`ceil(math:log2(M) * (1.0 / math:log2((1 + E) / (1 - E))))`.

For example, if you measure microseconds and you expect no operation to take more than an hour:
```erlang
1> F = fun(M, E) -> ceil(math:log2(M) * (1.0 / math:log2((1 + E) / (1 - E)))) end.
2> F(3_600_000_000, 0.01).
1101
```

Note than for an expected error of 1% and 2184 buckets, we can fit values
all the way as big to the biggest 64bits signed integer.
If we're measuring picoseconds, this would suffice to measure 107 days.

> #### When to use {: .tip}
> This is a good choice when it is kept in a persistent_term and it is globally shared.
> Note however all the perks and pitfalls of using a persistent term before choosing one.
>
> For example:
> ```erlang
> persistent_term:put(my_summary, ddskerl_counters:new(Opts)).
> ...
> ddskerl_counters:insert(persistent_term:get(my_summary), Value).
> ...
> ddskerl_counters:quantile(persistent_term:get(my_summary), Q).
> ```
""").

-export([new/1, total/1, insert/2, merge/2, quantile/2]).

-record(ddskerl_counters, {
    ref :: counters:counters_ref(),
    min_max :: atomics:atomics_ref(),
    width :: non_neg_integer(),
    bound :: non_neg_integer(),
    gamma :: float(),
    inv_log_gamma :: float()
}).

%% - total keeps track of the total count
%% - underflow of values that escape the summary below the minimum bucket: the interval (0,1]
%% - in between we find all the buckets
%% - overflow of values that escape the summary above the maximum bucket
-define(TOTAL_POS, 1).
-define(UNDERFLOW_POS, 2).
-define(EXTRA_KEYS, 3).
-define(OVERFLOW_POS(Bound), ?EXTRA_KEYS + Bound).
-define(MAX_INT, (1 bsl 64 - 1)).

?DOC("Options for the DDSketch").
-type opts() :: #{error := float(), bound := non_neg_integer()}.

?DOC("DDSketch instance").
-opaque ddsketch() :: #ddskerl_counters{}.

-export_type([ddsketch/0, opts/0]).

?DOC("Create a new DDSketch instance").
-spec new(opts()) -> ddsketch().
new(#{error := Err, bound := Bound}) ->
    %% We're going to keep thread-local mins and maxs, in order to avoid contention.
    %% Each scheduler gets two contiguous slots, in order to improve locality.
    %% Example: [MinSched1, MaxSched1, MinSched2, MaxSched2, ...]
    %% We write in the mins the highest 64bit unsigned integer and in the maxs the lowest,
    %% So that all future comparisons will succeed.
    Width = 2 * erlang:system_info(schedulers),
    MinMax = atomics:new(Width, [{signed, false}]),
    [atomics:put(MinMax, Ix, ?MAX_INT) || Ix <- lists:seq(1, Width, 2)],
    Ref = counters:new(?EXTRA_KEYS + Bound, [write_concurrency]),
    Gamma = (1 + Err) / (1 - Err),
    InvLogGamma = 1.0 / math:log2(Gamma),
    #ddskerl_counters{
        ref = Ref,
        min_max = MinMax,
        width = Width,
        bound = Bound,
        gamma = Gamma,
        inv_log_gamma = InvLogGamma
    }.

?DOC("Get the total number of elements in the DDSketch").
-spec total(ddsketch()) -> non_neg_integer().
total(#ddskerl_counters{ref = Ref}) ->
    counters:get(Ref, ?TOTAL_POS).

?DOC("Insert a value into the DDSketch").
-spec insert(ddsketch(), Value :: number()) -> ddsketch().
insert(#ddskerl_counters{ref = Ref, min_max = MinMax} = S, Val) when 0 < Val, Val =< 1 ->
    counters:add(Ref, ?TOTAL_POS, 1),
    counters:add(Ref, ?UNDERFLOW_POS, 1),
    update_min_max(MinMax, round(Val), erlang:system_info(scheduler_id)),
    S;
insert(
    #ddskerl_counters{ref = Ref, min_max = MinMax, bound = Bound, inv_log_gamma = InvLogGamma} = S,
    Val
) when
    1 < Val
->
    Key = ceil(math:log2(Val) * InvLogGamma),
    counters:add(Ref, ?TOTAL_POS, 1),
    update_min_max(MinMax, round(Val), erlang:system_info(scheduler_id)),
    case Key =< Bound of
        true ->
            counters:add(Ref, ?UNDERFLOW_POS + Key, 1);
        false ->
            counters:add(Ref, ?OVERFLOW_POS(Bound), 1)
    end,
    S.

-spec update_min_max(atomics:atomics_ref(), non_neg_integer(), non_neg_integer()) -> ok.
update_min_max(MinMax, Value, SchedulerId) ->
    MinIndex = 2 * SchedulerId - 1,
    MaxIndex = 2 * SchedulerId,
    Min = atomics:get(MinMax, MinIndex),
    Max = atomics:get(MinMax, MaxIndex),
    case {Value < Min, Max < Value} of
        {true, true} ->
            loop_min(MinMax, MinIndex, Min, Value),
            loop_max(MinMax, MaxIndex, Max, Value);
        {true, false} ->
            loop_min(MinMax, MinIndex, Min, Value);
        {false, true} ->
            loop_max(MinMax, MaxIndex, Max, Value);
        {false, false} ->
            ok
    end.

-spec loop_min(atomics:atomics_ref(), non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
    ok.
loop_min(MinMax, Index, Expected, Value) ->
    case atomics:compare_exchange(MinMax, Index, Expected, Value) of
        NewMin when is_integer(NewMin), Value < NewMin ->
            loop_min(MinMax, Index, NewMin, Value);
        _ ->
            ok
    end.

-spec loop_max(atomics:atomics_ref(), non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
    ok.
loop_max(MinMax, Index, Expected, Value) ->
    case atomics:compare_exchange(MinMax, Index, Expected, Value) of
        NewMax when is_integer(NewMax), NewMax < Value ->
            loop_max(MinMax, Index, NewMax, Value);
        _ ->
            ok
    end.

?DOC("Calculate the quantile of a DDSketch").
-spec quantile(ddsketch(), Quantile :: float()) -> Value :: float() | undefined.
quantile(#ddskerl_counters{min_max = MinMax, width = Width}, +0.0) ->
    Mins = [atomics:get(MinMax, Ix) || Ix <- lists:seq(1, Width, 2)],
    lists:min(Mins);
quantile(#ddskerl_counters{min_max = MinMax, width = Width}, 1.0) ->
    Maxs = [atomics:get(MinMax, Ix) || Ix <- lists:seq(2, Width, 2)],
    lists:max(Maxs);
quantile(#ddskerl_counters{ref = Ref, bound = Bound, gamma = Gamma}, Quantile) when
    0 < Quantile, Quantile < 1
->
    Total = counters:get(Ref, ?TOTAL_POS),
    TotalQuantile = Total * Quantile,
    AccumulatedRank = counters:get(Ref, ?UNDERFLOW_POS),
    get_quantile(Ref, TotalQuantile, Gamma, AccumulatedRank, ?EXTRA_KEYS, ?OVERFLOW_POS(Bound) + 1).

get_quantile(_, _, _, _, End, End) ->
    undefined;
get_quantile(Ref, TotalQuantile, Gamma, AccumulatedRank, Pos, OverflowPos) ->
    Value = counters:get(Ref, Pos),
    NewAccumulatedRank = AccumulatedRank + Value,
    case TotalQuantile =< NewAccumulatedRank of
        true ->
            result(Gamma, Pos - ?UNDERFLOW_POS);
        false ->
            get_quantile(Ref, TotalQuantile, Gamma, NewAccumulatedRank, Pos + 1, OverflowPos)
    end.

?DOC("Merge two DDSketch instances").
-spec merge(ddsketch(), ddsketch()) -> ddsketch().
merge(
    #ddskerl_counters{ref = Ref1, min_max = MinMax1, width = Width, bound = MaxBuckets, gamma = G} =
        S1,
    #ddskerl_counters{ref = Ref2, min_max = MinMax2, width = Width, bound = MaxBuckets, gamma = G}
) ->
    add_second_counter_into_first(Ref1, Ref2),
    add_second_atomic_into_first(MinMax1, MinMax2, Width),
    S1.

add_second_atomic_into_first(MinMax1, MinMax2, Width) ->
    [add_mins(MinMax1, MinMax2, Ix) || Ix <- lists:seq(1, Width, 2)],
    [add_maxs(MinMax1, MinMax2, Ix) || Ix <- lists:seq(2, Width, 2)],
    ok.

add_mins(MinMax1, MinMax2, Ix) ->
    Min1 = atomics:get(MinMax1, Ix),
    Min2 = atomics:get(MinMax2, Ix),
    Min2 < Min1 andalso loop_min(MinMax1, Ix, Min1, Min2).

add_maxs(MinMax1, MinMax2, Ix) ->
    Max1 = atomics:get(MinMax1, Ix),
    Max2 = atomics:get(MinMax2, Ix),
    Max1 < Max2 andalso loop_max(MinMax1, Ix, Max1, Max2).

add_second_counter_into_first(Ref1, Ref2) ->
    #{size := Size} = counters:info(Ref1),
    #{size := Size} = counters:info(Ref2),
    add_counters(Ref1, Ref2, Size, 1).

add_counters(_, _, Size, Pos) when Pos > Size ->
    ok;
add_counters(Ref1, Ref2, Size, Pos) ->
    Value2 = counters:get(Ref2, Pos),
    counters:add(Ref1, Pos, Value2),
    add_counters(Ref1, Ref2, Size, Pos + 1).

-spec result(number(), non_neg_integer()) -> number().
result(Gamma, Pos) ->
    2 * math:pow(Gamma, Pos) / (Gamma + 1).
