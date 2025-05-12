-module(ddskerl_protobuf).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.
?MODULEDOC("Protobuf encoding.").

-include("./proto/ddskerl_pb.hrl").
-include("./ddskerl.hrl").

-export([encode/1]).

?DOC("Encode any ddsketch to protobuf binary.").
-spec encode(_) -> binary().
encode(#ddskerl_std{data = Data, gamma = Gamma}) ->
    Msg = #'DDSketch'{
        positiveValues = #'Store'{binCounts = Data},
        mapping = #'IndexMapping'{gamma = Gamma},
        zeroCount = maps:get(0, Data, 0)
    },
    ddskerl_pb:encode_msg(Msg);
encode(#ddskerl_bound{data = Data, gamma = Gamma}) ->
    Bins = maps:from_list(gb_trees:to_list(Data)),
    Msg = #'DDSketch'{
        positiveValues = #'Store'{binCounts = Bins},
        mapping = #'IndexMapping'{gamma = Gamma}
    },
    ddskerl_pb:encode_msg(Msg);
encode(#ddskerl_ets{ref = Ref, name = Name}) ->
    [Element] = ets:lookup(Ref, Name),
    Gamma = element(?E_GAMMA_POS, Element),
    Bins = lists:foldl(
        fun(N, Acc) ->
            case element(N, Element) of
                0 -> Acc;
                Value -> Acc#{(N - ?E_PREFIX) => Value}
            end
        end,
        #{},
        lists:seq(?E_PREFIX, tuple_size(Element))
    ),
    Msg = #'DDSketch'{
        positiveValues = #'Store'{binCounts = Bins},
        mapping = #'IndexMapping'{gamma = Gamma},
        zeroCount = element(?E_UNDERFLOW_POS, Element)
    },
    ddskerl_pb:encode_msg(Msg);
encode(#ddskerl_counters{ref = Ref, gamma = Gamma, bound = Bound}) ->
    ToIndex = ?C_OVERFLOW_POS(Bound),
    Bins = lists:foldl(
        fun(N, Acc) ->
            case counters:get(Ref, N) of
                0 -> Acc;
                Value -> Acc#{(N - ?C_PREFIX) => Value}
            end
        end,
        #{},
        lists:seq(?C_PREFIX, ToIndex)
    ),
    Msg = #'DDSketch'{
        positiveValues = #'Store'{binCounts = Bins},
        mapping = #'IndexMapping'{gamma = Gamma},
        zeroCount = counters:get(Ref, ?C_UNDERFLOW_POS)
    },
    ddskerl_pb:encode_msg(Msg).
