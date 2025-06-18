-ifndef('DDSKERL_HRL').

-record(ddskerl_exact, {
    data = #{} :: #{number() => non_neg_integer()},
    total = 0 :: non_neg_integer(),
    sum = 0 :: number()
}).

-record(ddskerl_std, {
    data = #{} :: #{non_neg_integer() => non_neg_integer()},
    total = 0 :: non_neg_integer(),
    sum = 0 :: number(),
    min :: undefined | number(),
    max = 0 :: number(),
    gamma :: float(),
    inv_log_gamma :: float()
}).

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

%% Total keeps track of the total count
%% overflow of values that escape the summary above the maximum bucket
%% underflow of values that escape the summary below the minimum bucket
-define(E_BOUND_POS, 2).
-define(E_GAMMA_POS, 3).
-define(E_INV_LOG_GAMMA_POS, 4).
-define(E_MIN_POS, 5).
-define(E_MAX_POS, 6).
-define(E_TOTAL_POS, 7).
-define(E_SUM_POS, 8).
-define(E_OVERFLOW_POS, 9).
-define(E_UNDERFLOW_POS, 10).
-define(E_PREFIX, 10).
-define(E_MIN_INT, (0)).
-define(E_MAX_INT, (1 bsl 64 - 1)).

-record(ddskerl_ets, {
    ref :: ets:tab(),
    name :: term()
}).

%% - total keeps track of the total count
%% - underflow of values that escape the summary below the minimum bucket: the interval (0,1]
%% - in between we find all the buckets
%% - overflow of values that escape the summary above the maximum bucket
-define(C_TOTAL_POS, 1).
-define(C_SUM_POS, 2).
-define(C_UNDERFLOW_POS, 3).
-define(C_EXTRA_KEYS, 4).
-define(C_PREFIX, 3).
-define(C_OVERFLOW_POS(Bound), ?C_EXTRA_KEYS + Bound).
-define(C_MAX_INT, (1 bsl 64 - 1)).

-record(ddskerl_counters, {
    ref :: counters:counters_ref(),
    min_max :: atomics:atomics_ref(),
    width :: non_neg_integer(),
    bound :: non_neg_integer(),
    gamma :: float(),
    inv_log_gamma :: float()
}).

-endif.
