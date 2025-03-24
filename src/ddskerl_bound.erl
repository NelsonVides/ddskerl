-module(ddskerl_bound).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.
?MODULEDOC("""
DDSketch implementation in Erlang.

This implements an bounded bucket count, that is, on a degenerate case,
memory consumption won't grow but lower quantiles will lose accuracy as the summary saturates.

When the number of buckets exceeds the limit,
the two smallest buckets are collapsed into one and the limit is kept.

The implementation uses a local and non-shared `m:gb_trees`, and the buckets are not preallocated,
but are rather created on demand, without bounds on their indexes.

Because the data structure cannot be accessed concurretly (share-nothing semantics),
a good strategy would be to keep many running in parallel, and running queries over a merge.

> #### When to use {: .tip}
> This is a good choice when the summary is tracked by a process
> and updates the state through its mailbox.
>
> For example, the following could be used as a template:
> ```
> ...
> -behaviour(gen_server).
>
> -spec start_link(ddskerl_bound:opts()) -> gen_server:start_ret().
start_link(Opts) ->
>    gen_server:start_link(?MODULE, Opts, [{spawn_opt, [{message_queue_data, off_heap}]}]).
>
> -spec init(ddskerl_bound:opts()) -> {ok, ddskerl_bound:ddsketch()}.
> init(Opts) ->
>    {ok, ddskerl_bound:new(Opts)}.
>
> handle_call({get_quantile, Q}, _From, Sketch) ->
>    {reply, ddskerl_bound:quantile(Sketch, Q), Sketch}.
>
> handle_cast({new_value, Value}, Sketch) ->
>    {noreply, ddskerl_bound:insert(Sketch, Value)}.
> ...
> ```
""").

-behaviour(ddskerl).

-export([new/1, total/1, sum/1, insert/2, merge/2, quantile/2]).

-record(ddskerl_bound, {
    data = gb_trees:empty() :: gb_trees:tree(non_neg_integer(), non_neg_integer()),
    total = 0 :: non_neg_integer(),
    min :: undefined | number(),
    max = 0 :: number(),
    sum = 0 :: number(),
    bound :: non_neg_integer(),
    gamma :: float(),
    inv_log_gamma :: float()
}).

?DOC("Options for the DDSketch.").
-type opts() :: #{error := float(), bound := non_neg_integer()}.

?DOC("DDSketch instance.").
-opaque ddskerl_bound() :: #ddskerl_bound{}.

-export_type([ddskerl_bound/0, opts/0]).

?DOC("Create a new DDSketch instance.").
-spec new(opts()) -> ddskerl_bound().
new(#{error := Err, bound := Bound}) ->
    Gamma = (1 + Err) / (1 - Err),
    InvLogGamma = 1.0 / math:log2(Gamma),
    #ddskerl_bound{bound = Bound, gamma = Gamma, inv_log_gamma = InvLogGamma}.

?DOC("Get the total number of elements in the DDSketch.").
-spec total(ddskerl_bound()) -> non_neg_integer().
total(#ddskerl_bound{total = Total}) ->
    Total.

?DOC("Get the sum number of elements in the DDSketch.").
-spec sum(ddskerl_bound()) -> number().
sum(#ddskerl_bound{sum = Sum}) ->
    Sum.

?DOC("Insert a value into the DDSketch.").
-spec insert(ddskerl_bound(), number()) -> ddskerl_bound().
insert(
    #ddskerl_bound{
        data = Data,
        total = Total,
        sum = Sum,
        min = Min,
        max = Max,
        inv_log_gamma = InvLogGamma,
        bound = Bound
    } = S,
    Val
) when Val > 0 ->
    Key = ceil(math:log2(Val) * InvLogGamma),
    NewData =
        case gb_trees:is_defined(Key, Data) of
            true -> gb_trees:update(Key, gb_trees:get(Key, Data) + 1, Data);
            false -> gb_trees:insert(Key, 1, Data)
        end,
    case gb_trees:size(NewData) =< Bound of
        true ->
            S#ddskerl_bound{
                data = NewData,
                total = Total + 1,
                sum = Sum + Val,
                min = min(Min, Val),
                max = max(Max, Val)
            };
        false ->
            % Handle the case where the number of buckets exceeds the limit
            % by collapsing the smallest buckets
            {_MinKey, Value, TrimmedData} = gb_trees:take_smallest(NewData),
            {MinKey2, Value2} = gb_trees:smallest(TrimmedData),
            S#ddskerl_bound{
                data = gb_trees:update(MinKey2, Value + Value2, TrimmedData),
                total = Total + 1,
                sum = Sum + Val,
                min = min(Min, Val),
                max = max(Max, Val)
            }
    end.

?DOC("Calculate the quantile of a DDSketch.").
-spec quantile(ddskerl_bound(), float()) -> float() | undefined.
quantile(#ddskerl_bound{min = Min}, +0.0) ->
    Min;
quantile(#ddskerl_bound{max = Max}, 1.0) ->
    Max;
quantile(#ddskerl_bound{data = Data, total = Total, gamma = Gamma}, Quantile) when
    0.0 < Quantile, Quantile < 1.0
->
    TotalQuantile = Total * Quantile,
    get_quantile(Data, TotalQuantile, 0, gb_trees:next(gb_trees:iterator(Data)), Gamma).

get_quantile(_, _, _, none, _) ->
    % Should not happen if Total > 0
    undefined;
get_quantile(Data, TotalQuantile, AccumulatedRank, {Key, Count, NextIter}, Gamma) ->
    NewAccumulatedRank = AccumulatedRank + Count,
    case TotalQuantile =< NewAccumulatedRank of
        true ->
            2 * math:pow(Gamma, Key) / (Gamma + 1);
        false ->
            get_quantile(Data, TotalQuantile, NewAccumulatedRank, gb_trees:next(NextIter), Gamma)
    end.

?DOC("Merge two DDSketch instances.").
-spec merge(ddskerl_bound(), ddskerl_bound()) -> ddskerl_bound().
merge(
    #ddskerl_bound{
        data = Data1, total = Total1, sum = Sum1, bound = Bound, gamma = G, min = Min1, max = Max1
    } =
        S1,
    #ddskerl_bound{
        data = Data2, total = Total2, sum = Sum2, bound = Bound, gamma = G, min = Min2, max = Max2
    }
) ->
    MergedData = merge_trees(Data1, Data2),
    FinalData = trim_to_max_buckets(MergedData, Bound),
    S1#ddskerl_bound{
        data = FinalData,
        total = Total1 + Total2,
        sum = Sum1 + Sum2,
        min = min(Min1, Min2),
        max = max(Max1, Max2)
    }.

merge_trees(Data1, Data2) ->
    Iterator = gb_trees:next(gb_trees:iterator(Data2)),
    do_merge_trees(Data1, Iterator).

do_merge_trees(Data, none) ->
    Data;
do_merge_trees(Data, {Key, Count, NextIter}) ->
    NewData =
        case gb_trees:is_defined(Key, Data) of
            true -> gb_trees:update(Key, gb_trees:get(Key, Data) + Count, Data);
            false -> gb_trees:insert(Key, Count, Data)
        end,
    do_merge_trees(NewData, gb_trees:next(NextIter)).

trim_to_max_buckets(Data, Bound) ->
    case gb_trees:size(Data) =< Bound of
        true ->
            Data;
        false ->
            {_MinKey, Value, TrimmedData} = gb_trees:take_smallest(Data),
            {MinKey2, Value2} = gb_trees:smallest(TrimmedData),
            CollapsedOneBucket = gb_trees:update(MinKey2, Value + Value2, TrimmedData),
            trim_to_max_buckets(CollapsedOneBucket, Bound)
    end.
