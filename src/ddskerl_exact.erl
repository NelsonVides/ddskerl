-module(ddskerl_exact).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.
?MODULEDOC("""
Quantile summary exact implemenation.

This implements an exact count of all elements seen,
which is the slowest and most memory consuming algorithm.

It is on the other hand an exact algorithm,
hence it can be used for comparison against all other approximations.
""").

-include("./ddskerl.hrl").

-export([new/0, total/1, sum/1, insert/2, merge/2, quantile/2]).

?DOC("DDSketch instance.").
-opaque ddsketch() :: #ddskerl_exact{}.

-export_type([ddsketch/0]).

?DOC("Create a new DDSketch instance.").
-spec new() -> ddsketch().
new() ->
    #ddskerl_exact{}.

?DOC("Get the total number of elements in the DDSketch.").
-spec total(ddsketch()) -> non_neg_integer().
total(#ddskerl_exact{total = Total}) ->
    Total.

?DOC("Get the sum of elements in the DDSketch.").
-spec sum(ddsketch()) -> non_neg_integer().
sum(#ddskerl_exact{sum = Sum}) ->
    Sum.

?DOC("Insert a value into the DDSketch.").
-spec insert(ddsketch(), number()) -> ddsketch().
insert(#ddskerl_exact{data = Data, total = Total, sum = Sum} = S, Val) when 0 < Val ->
    NewData = maps:update_with(Val, fun(X) -> X + 1 end, 1, Data),
    S#ddskerl_exact{data = NewData, total = Total + 1, sum = Sum + Val}.

?DOC("Merge two DDSketch instances.").
-spec merge(ddsketch(), ddsketch()) -> ddsketch().
merge(
    #ddskerl_exact{data = Data1, total = Total1, sum = Sum1},
    #ddskerl_exact{data = Data2, total = Total2, sum = Sum2}
) ->
    Data = maps:merge_with(fun(_K, Val1, Val2) -> Val1 + Val2 end, Data1, Data2),
    #ddskerl_exact{data = Data, total = Total1 + Total2, sum = Sum1 + Sum2}.

?DOC("Calculate the quantile of a DDSketch.").
-spec quantile(ddsketch(), number()) -> number() | undefined.
quantile(#ddskerl_exact{data = Data, total = Total}, Quantile) when 0 =< Quantile, Quantile =< 1 ->
    TotalQuantile = Total * Quantile,
    SortedData = lists:sort(fun({Key1, _}, {Key2, _}) -> Key1 =< Key2 end, maps:to_list(Data)),
    try
        lists:foldl(
            fun({Key, Val}, Acc) ->
                NewAccumulatedRank = Acc + Val,
                case TotalQuantile =< NewAccumulatedRank of
                    true ->
                        throw({found, Key});
                    false ->
                        NewAccumulatedRank
                end
            end,
            0,
            SortedData
        )
    of
        _ -> undefined
    catch
        throw:{found, Key} -> Key
    end.
