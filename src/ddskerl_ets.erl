-module(ddskerl_ets).
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

In this optimisation, buckets are preallocated using `m:ets` tables.
Values within `(0,1)` will be inserted into the underflow bucket;
values above the expected limit will be inserted into the overflow bucket.

## Calculating the bucket size
If you have an expected highest value `M` and a given error `E`,
your ideal bucket size would be given by the formula:
`ceil(math:log2(M) * (1.0 / math:log2((1 + E) / (1 - E))))`.

For example, if you measure microseconds and you expect no operation to take more than an hour:
```erlang
1> F = fun(M, E) -> ceil(math:log2(M) * (1.0 / math:log2((1 + E) / (1 - E)))) end.
2> F(3600000000, 0.01).
1101
```

Note than for an expected error of 1% and 2184 buckets, we can fit values
all the way as big to the biggest 64bits signed integer.
If we're measuring picoseconds, this would suffice to measure 107 days.

> #### When to use {: .tip}
> This is a good choice when shared ets tables are a more efficient way to ensure dynamicity
> and potentially code upgrades.
>
> For example:
> ```erlang
> ddskerl_ets:new(#{ets_table => Table, name => Name, ...}).
> ...
> ddskerl_ets:insert(Table, Name, Value).
> ...
> ddskerl_ets:quantile(Table, Name, Q).
> ```
""").

-behaviour(ddskerl).

-export([
    new/1, new/4,
    total/1, total/2,
    sum/1, sum/2,
    insert/2, insert/3,
    merge/2, merge/4,
    reset/1, reset/2,
    quantile/2, quantile/3
]).

-record(ddskerl_ets, {
    ref :: ets:tab(),
    name :: term()
}).

%% Total keeps track of the total count
%% overflow of values that escape the summary above the maximum bucket
%% underflow of values that escape the summary below the minimum bucket
%-define(NAME_POS, 1).
-define(BOUND_POS, 2).
-define(GAMMA_POS, 3).
-define(INV_LOG_GAMMA_POS, 4).
-define(TOTAL_POS, 5).
-define(SUM_POS, 6).
-define(MIN_POS, 7).
-define(MAX_POS, 8).
-define(OVERFLOW_POS, 9).
-define(UNDERFLOW_POS, 10).
-define(PREFIX, 10).
-define(MIN_INT, (0)).
-define(MAX_INT, (1 bsl 64 - 1)).

?DOC("""
Options for the DDSketch.

`ets_table` refers to the table weher the sketch is stored,
`name` is the key to use when storing the sketch in the ets table.
""").
-type opts() :: #{
    ets_table := ets:tab(),
    name := term(),
    error := float(),
    bound := non_neg_integer()
}.

?DOC("DDSketch instance.").
-opaque ddsketch() :: #ddskerl_ets{}.
?DOC("DDSketch tuple.").
-opaque object() :: tuple().

-export_type([ddsketch/0, opts/0, object/0]).

?DOC("Create a new DDSketch instance.").
-spec new(opts()) -> ddsketch().
new(#{ets_table := Ref, name := Name, error := Err, bound := Bound}) ->
    new(Ref, Name, Err, Bound),
    #ddskerl_ets{ref = Ref, name = Name}.

?DOC("Create a new DDSketch instance.").
-spec new(ets:table(), term(), float(), non_neg_integer()) -> boolean().
new(Ref, Name, Err, Bound) ->
    Gamma = (1 + Err) / (1 - Err),
    InvLogGamma = 1.0 / math:log2(Gamma),
    Object = create_object(Name, Bound, Gamma, InvLogGamma),
    ets:insert_new(Ref, Object).

?DOC("Get the total number of elements in the DDSketch.").
-spec total(ddsketch()) -> non_neg_integer().
total(#ddskerl_ets{ref = Ref, name = Name}) ->
    total(Ref, Name).

?DOC("Get the total number of elements in the DDSketch.").
-spec total(ets:table(), term()) -> non_neg_integer().
total(Ref, Name) ->
    ets:lookup_element(Ref, Name, ?TOTAL_POS).

?DOC("Get the sum of elements in the DDSketch.").
-spec sum(ddsketch()) -> non_neg_integer().
sum(#ddskerl_ets{ref = Ref, name = Name}) ->
    sum(Ref, Name).

?DOC("Get the sum of elements in the DDSketch.").
-spec sum(ets:table(), term()) -> non_neg_integer().
sum(Ref, Name) ->
    ets:lookup_element(Ref, Name, ?SUM_POS).

?DOC("Reset the DDSketch values to zero").
-spec reset(ddsketch()) -> ddsketch().
reset(#ddskerl_ets{ref = Ref, name = Name} = S) ->
    reset(Ref, Name),
    S.

?DOC("Reset the DDSketch values to zero").
-spec reset(ets:tab(), term()) -> boolean().
reset(Ref, Name) ->
    Gamma = ets:lookup_element(Ref, Name, ?GAMMA_POS),
    Bound = ets:lookup_element(Ref, Name, ?BOUND_POS),
    InvLogGamma = ets:lookup_element(Ref, Name, ?INV_LOG_GAMMA_POS),
    ets:insert(Ref, create_object(Name, Bound, Gamma, InvLogGamma)).

?DOC("Insert a value into the DDSketch.").
-spec insert(ddsketch(), number()) -> ddsketch().
insert(#ddskerl_ets{ref = Ref, name = Name} = S, Val) ->
    insert(Ref, Name, Val),
    S.

?DOC("Insert a value into the DDSketch.").
-spec insert(ets:tab(), term(), number()) -> any().
insert(Ref, Name, Val) when 0 < Val, Val =< 1 ->
    Spec = [{?TOTAL_POS, 1}, {?UNDERFLOW_POS, 1}],
    update_min_max_sum(Ref, Name, Val),
    ets:update_counter(Ref, Name, Spec);
insert(Ref, Name, Val) when
    1 < Val
->
    Bound = ets:lookup_element(Ref, Name, ?BOUND_POS),
    InvLogGamma = ets:lookup_element(Ref, Name, ?INV_LOG_GAMMA_POS),
    Key = ceil(math:log2(Val) * InvLogGamma),
    update_min_max_sum(Ref, Name, Val),
    case Key =< Bound of
        true ->
            Spec = [{?TOTAL_POS, 1}, {?PREFIX + Key, 1}],
            ets:update_counter(Ref, Name, Spec);
        false ->
            Spec = [{?TOTAL_POS, 1}, {?OVERFLOW_POS, 1}],
            ets:update_counter(Ref, Name, Spec)
    end.

-spec update_min_max_sum(ets:tab(), term(), non_neg_integer()) -> any().
update_min_max_sum(Ref, Name, Value) ->
    ets:update_element(Ref, Name, [{?SUM_POS, ets:lookup_element(Ref, Name, ?SUM_POS) + Value}]),
    Min = ets:lookup_element(Ref, Name, ?MIN_POS),
    Value < Min andalso ets:update_element(Ref, Name, [{?MIN_POS, Value}]),
    Max = ets:lookup_element(Ref, Name, ?MAX_POS),
    Max < Value andalso ets:update_element(Ref, Name, [{?MAX_POS, Value}]).

?DOC("Calculate the quantile of a DDSketch.").
-spec quantile(ddsketch(), float()) -> float() | undefined.
quantile(#ddskerl_ets{ref = Ref, name = Name}, Q) ->
    quantile(Ref, Name, Q).

?DOC("Calculate the quantile of a DDSketch.").
-spec quantile(ets:tab(), term(), float()) -> float() | undefined.
quantile(Ref, Name, +0.0) ->
    ets:lookup_element(Ref, Name, ?MIN_POS);
quantile(Ref, Name, 1.0) ->
    ets:lookup_element(Ref, Name, ?MAX_POS);
quantile(Ref, Name, Quantile) when
    0 < Quantile, Quantile < 1
->
    [Element] = ets:lookup(Ref, Name),
    Gamma = element(?GAMMA_POS, Element),
    Total = element(?TOTAL_POS, Element),
    AccumulatedRank = element(?UNDERFLOW_POS, Element),
    TotalQuantile = Total * Quantile,
    case TotalQuantile =< AccumulatedRank of
        true ->
            result(Gamma, -?MAX_INT);
        false ->
            get_quantile(
                Element, TotalQuantile, Gamma, AccumulatedRank, ?PREFIX + 1, tuple_size(Element) + 2
            )
    end.

get_quantile(_, _, _, _, OverEnd, OverEnd) ->
    undefined;
get_quantile(Element, TotalQuantile, Gamma, AccumulatedRank, Pos, OverflowPos) ->
    Value = element(Pos, Element),
    NewAccumulatedRank = AccumulatedRank + Value,
    case TotalQuantile =< NewAccumulatedRank of
        true ->
            result(Gamma, Pos - ?PREFIX);
        false ->
            get_quantile(Element, TotalQuantile, Gamma, NewAccumulatedRank, Pos + 1, OverflowPos)
    end.

?DOC("Merge two DDSketch instances.").
-spec merge(ddsketch(), ddsketch()) -> ddsketch().
merge(#ddskerl_ets{ref = Ref1, name = Name1} = S1, #ddskerl_ets{ref = Ref2, name = Name2}) ->
    merge(Ref1, Name1, Ref2, Name2),
    S1.

?DOC("Merge two DDSketch instances.").
-spec merge(ets:tab(), term(), ets:tab(), term()) -> any().
merge(Ref1, Name1, Ref2, Name2) ->
    verify_compatible(Ref1, Name1, Ref2, Name2),
    merge_totals(Ref1, Name1, Ref2, Name2),
    merge_minimum(Ref1, Name1, Ref2, Name2),
    merge_maximum(Ref1, Name1, Ref2, Name2),
    merge_counts(Ref1, Name1, Ref2, Name2).

merge_counts(Ref1, Name1, Ref2, Name2) ->
    Bound = ets:lookup_element(Ref1, Name1, ?BOUND_POS),
    lists:foreach(
        fun(Pos) ->
            Value1 = ets:lookup_element(Ref1, Name1, Pos),
            Value2 = ets:lookup_element(Ref2, Name2, Pos),
            Spec1 = [{Pos, Value1 + Value2}],
            ets:update_element(Ref1, Name1, Spec1)
        end,
        lists:seq(?PREFIX, Bound)
    ).

merge_maximum(Ref1, Name1, Ref2, Name2) ->
    Value1 = ets:lookup_element(Ref1, Name1, ?MAX_POS),
    Value2 = ets:lookup_element(Ref2, Name2, ?MAX_POS),
    Spec1 = [{?MAX_POS, max(Value1, Value2)}],
    ets:update_element(Ref1, Name1, Spec1).

merge_minimum(Ref1, Name1, Ref2, Name2) ->
    Value1 = ets:lookup_element(Ref1, Name1, ?MIN_POS),
    Value2 = ets:lookup_element(Ref2, Name2, ?MIN_POS),
    Spec1 = [{?MIN_POS, min(Value1, Value2)}],
    ets:update_element(Ref1, Name1, Spec1).

merge_totals(Ref1, Name1, Ref2, Name2) ->
    Value1 = ets:lookup_element(Ref1, Name1, ?TOTAL_POS),
    Value2 = ets:lookup_element(Ref2, Name2, ?TOTAL_POS),
    Spec1 = [{?TOTAL_POS, Value1 + Value2}],
    ets:update_element(Ref1, Name1, Spec1).

verify_compatible(Ref1, Name1, Ref2, Name2) ->
    Bound = ets:lookup_element(Ref1, Name1, ?BOUND_POS),
    Bound = ets:lookup_element(Ref2, Name2, ?BOUND_POS),
    Gamma = ets:lookup_element(Ref1, Name1, ?GAMMA_POS),
    Gamma = ets:lookup_element(Ref2, Name2, ?GAMMA_POS).

-spec create_object(term(), pos_integer(), float(), float()) -> object().
create_object(Name, Bound, Gamma, InvLogGamma) ->
    Header = [Name, Bound, Gamma, InvLogGamma, 0, 0, ?MAX_INT, ?MIN_INT, 0, 0],
    Counters = lists:duplicate(Bound, 0),
    Object = Header ++ Counters,
    list_to_tuple(Object).

-spec result(number(), integer()) -> number().
result(_, 0) ->
    0.0;
result(Gamma, Pos) ->
    2 * math:pow(Gamma, Pos) / (Gamma + 1).
