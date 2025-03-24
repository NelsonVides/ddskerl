-module(ddskerl_std).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.
?MODULEDOC("""
DDSketch implementation in Erlang.

This implements an unbounded bucket count, that is, on a degenerate case,
the memory consumption will be unbounded logarithmic on the number of different elements.

The underlying data structure is a map. Note however, that, if measuring picoseconds,
a map with 2184 buckets would keep track of all values all the way to 107 days.

Because the data structure cannot be accessed concurretly (share-nothing semantics),
a good strategy would be to keep many running in parallel, and running queries over a merge.

> #### When to use {: .tip}
> This is a good choice when the summary is tracked by a process
> and updates the state through its mailbox.
>
> For example, the following could be used as a template:
> ```erlang
> ...
> -behaviour(gen_server).
>
> -spec start_link(ddskerl_std:opts()) -> gen_server:start_ret().
start_link(Opts) ->
>    gen_server:start_link(?MODULE, Opts, [{spawn_opt, [{message_queue_data, off_heap}]}]).
>
> -spec init(ddskerl_std:opts()) -> {ok, ddskerl_std:ddsketch()}.
> init(Opts) ->
>    {ok, ddskerl_std:new(Opts)}.
>
> handle_call({get_quantile, Q}, _From, Sketch) ->
>    {reply, ddskerl_std:quantile(Sketch, Q), Sketch}.
>
> handle_cast({new_value, Value}, Sketch) ->
>    {noreply, ddskerl_std:insert(Sketch, Value)}.
> ...
> ```
""").

-behaviour(ddskerl).

-export([new/1, total/1, sum/1, insert/2, merge/2, quantile/2]).

-record(ddskerl_std, {
    data = #{} :: #{non_neg_integer() => non_neg_integer()},
    total = 0 :: non_neg_integer(),
    sum = 0 :: number(),
    min :: undefined | number(),
    max = 0 :: number(),
    gamma :: float(),
    inv_log_gamma :: float()
}).

?DOC("Options for the DDSketch.").
-type opts() :: #{error := float(), _ => _}.

?DOC("DDSketch instance.").
-opaque ddsketch() :: #ddskerl_std{}.

-export_type([ddsketch/0, opts/0]).

?DOC("Create a new DDSketch instance.").
-spec new(opts()) -> ddsketch().
new(#{error := Err}) ->
    Gamma = (1 + Err) / (1 - Err),
    InvLogGamma = 1.0 / math:log2(Gamma),
    #ddskerl_std{gamma = Gamma, inv_log_gamma = InvLogGamma}.

?DOC("Get the total number of elements in the DDSketch.").
-spec total(ddsketch()) -> non_neg_integer().
total(#ddskerl_std{total = Total}) ->
    Total.

?DOC("Get the sum of elements in the DDSketch.").
-spec sum(ddsketch()) -> number().
sum(#ddskerl_std{sum = Sum}) ->
    Sum.

?DOC("Insert a value into the DDSketch.").
-spec insert(ddsketch(), number()) -> ddsketch().
insert(
    #ddskerl_std{
        data = Data, total = Total, sum = Sum, min = Min, max = Max, inv_log_gamma = InvLogGamma
    } = S,
    Val
) when
    Val > 0
->
    Bin = ceil(math:log2(Val) * InvLogGamma),
    NewData = maps:update_with(Bin, fun(X) -> X + 1 end, 1, Data),
    S#ddskerl_std{
        data = NewData, total = Total + 1, sum = Sum + Val, min = min(Min, Val), max = max(Max, Val)
    }.

?DOC("Calculate the quantile of a DDSketch.").
-spec quantile(ddsketch(), float()) -> float() | undefined.
quantile(#ddskerl_std{min = Min}, +0.0) ->
    Min;
quantile(#ddskerl_std{max = Max}, 1.0) ->
    Max;
quantile(#ddskerl_std{data = Data, gamma = Gamma, total = Total}, Quantile) when
    0.0 < Quantile, Quantile < 1.0
->
    AccumulatedRank = 0,
    TotalQuantile = Total * Quantile,
    case TotalQuantile =< AccumulatedRank of
        true ->
            result(Gamma, 0);
        false ->
            get_quantile(Data, TotalQuantile, AccumulatedRank, Data, Gamma)
    end.

get_quantile(Data, TotalQuantile, AccumulatedRank, Data, Gamma) ->
    SortedData = lists:sort(fun({Pos1, _}, {Pos2, _}) -> Pos1 =< Pos2 end, maps:to_list(Data)),
    try
        lists:foldl(
            fun({Pos, Val}, Acc) ->
                NewAccumulatedRank = Acc + Val,
                case TotalQuantile =< NewAccumulatedRank of
                    true ->
                        throw({found, Pos});
                    false ->
                        NewAccumulatedRank
                end
            end,
            AccumulatedRank,
            SortedData
        )
    of
        _ -> undefined
    catch
        throw:{found, Pos} ->
            result(Gamma, Pos)
    end.

?DOC("Merge two DDSketch instances.").
-spec merge(ddsketch(), ddsketch()) -> ddsketch().
merge(
    #ddskerl_std{gamma = G, data = Data1, total = Total1, sum = Sum1, min = Min1, max = Max1} = S1,
    #ddskerl_std{gamma = G, data = Data2, total = Total2, sum = Sum2, min = Min2, max = Max2}
) ->
    Data = maps:merge_with(fun(_K, Val1, Val2) -> Val1 + Val2 end, Data1, Data2),
    S1#ddskerl_std{
        data = Data,
        total = Total1 + Total2,
        sum = Sum1 + Sum2,
        min = min(Min1, Min2),
        max = max(Max1, Max2)
    }.

-compile({inline, [result/2]}).
-spec result(number(), non_neg_integer()) -> float().
result(_, 0) ->
    0.0;
result(Gamma, Pos) ->
    2 * math:pow(Gamma, Pos) / (Gamma + 1).
