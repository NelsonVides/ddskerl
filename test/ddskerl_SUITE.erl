-module(ddskerl_SUITE).
-compile([export_all, nowarn_export_all]).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-include_lib("proper/include/proper.hrl").

%% Exported functions
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test suite configuration
-spec all() -> [ct_suite:ct_test_def()].
all() ->
    [
        {group, ddskerl_std},
        {group, ddskerl_bound}
    ].

-spec groups() -> [ct_suite:ct_group_def()].
groups() ->
    [
        {ddskerl_std, tests()},
        {ddskerl_bound, tests()}
    ].

-spec tests() -> [atom()].
tests() ->
    [get_quantile_test, merge_test, prop_quantile_error_test].

-spec init_per_suite(ct_suite:ct_config()) -> ct_suite:ct_config().
init_per_suite(Config) ->
    Config.

-spec end_per_suite(ct_suite:ct_config()) -> term().
end_per_suite(_Config) ->
    ok.

-spec init_per_group(ct_suite:ct_groupname(), ct_suite:ct_config()) -> ct_suite:ct_config().
init_per_group(GroupModule, Config) ->
    [{module, GroupModule} | Config].

-spec end_per_group(ct_suite:ct_groupname(), ct_suite:ct_config()) -> term().
end_per_group(_Group, _Config) ->
    ok.

-spec init_per_testcase(ct_suite:ct_testcase(), ct_suite:ct_config()) -> ct_suite:ct_config().
init_per_testcase(TestCase, Config) ->
    ets:new(TestCase, [public, named_table]),
    Config.

-spec end_per_testcase(ct_suite:ct_testcase(), ct_suite:ct_config()) -> term().
end_per_testcase(_TestCase, _Config) ->
    ok.

%% Test cases
get_quantile_test(Config) ->
    Module = proplists:get_value(module, Config),
    Sample = [4, 8],
    RelativeError = 0.02,
    Bound = bucket_size(10, RelativeError),
    Sketch = estimate_sketch(Module, Sample, RelativeError, Bound),
    Quantile = Module:quantile(Sketch, 0.5),
    ?assert(
        abs(Quantile - 4.0) =< RelativeError * 4.0,
        #{quantile => Quantile, sketch => Sketch}
    ).

merge_test(Config) ->
    Module = proplists:get_value(module, Config),
    RelativeError = 0.02,
    Bound = bucket_size(10, RelativeError),
    Sketch1 = estimate_sketch(Module, [4], RelativeError, Bound),
    Sketch2 = estimate_sketch(Module, [8], RelativeError, Bound),
    MergedSketch = Module:merge(Sketch1, Sketch2),
    Quantile = Module:quantile(MergedSketch, 0.5),
    ?assert(
        abs(Quantile - 4.0) =< RelativeError * 4.0,
        #{sketch_one => Sketch1, sketch_two => Sketch2}
    ).

%% Run the property-based test
prop_quantile_error_test(Config) ->
    Module = proplists:get_value(module, Config),
    Opts = [
        quiet,
        noshrink,
        long_result,
        {start_size, 5},
        {max_size, 1_000_000},
        {numtests, 100},
        {numworkers, erlang:system_info(schedulers_online)}
    ],
    case proper:counterexample(prop_quantile(Module, ?FUNCTION_NAME), Opts) of
        true ->
            ct:comment("Property check for ~s passed", [Module]);
        [{RelativeError, Sample} | _] ->
            Bound = bucket_size(1 + lists:max(Sample), RelativeError),
            EstimatedSketch = estimate_sketch(Module, Sample, RelativeError, Bound),
            ActualSketch = calculate_sketch(Module, Sample, RelativeError, Bound),
            Properties = lists:map(
                fun(Q) ->
                    SketchQuantile = Module:quantile(EstimatedSketch, Q),
                    ActualQuantile = ddskerl_exact:quantile(ActualSketch, Q),
                    Property =
                        abs(SketchQuantile / ActualQuantile - 1) =< RelativeError,
                    {Q, SketchQuantile, ActualQuantile, Property}
                end,
                quantiles()
            ),
            Format = "-properties: ~w~n-sketch: ~p~n-sample: ~p~n",
            Values = [Properties, EstimatedSketch, Sample],
            ct:pal(Format, Values),
            ct:fail(
                "Failed with relative error ~p and sample size ~B, see logs for more details",
                [RelativeError, length(Sample)]
            )
    end.

%% Property-based test for quantile error
prop_quantile(Module, T) ->
    ?FORALL(
        {RelativeError, Sample},
        {relative_error(), sample(Module)},
        prop(Module, T, RelativeError, Sample)
    ).

prop(Module, T, RelativeError, Sample) ->
    SampleSize = length(Sample),
    Bound = bucket_size(1 + lists:max(Sample), RelativeError),
    EstimatedSketch = estimate_sketch(Module, Sample, RelativeError, Bound),
    ActualSketch = calculate_sketch(Module, Sample, RelativeError, Bound),
    lists:all(
        fun(Q) ->
            check_quantile(EstimatedSketch, ActualSketch, Module, RelativeError, SampleSize, Q)
        end,
        quantiles()
    ).

check_quantile(EstimatedSketch, ActualSketch, Module, RelativeError, SampleSize, Q) ->
    SketchQuantile = Module:quantile(EstimatedSketch, Q),
    ActualQuantile = ddskerl_exact:quantile(ActualSketch, Q),
    abs(SketchQuantile - ActualQuantile) =< RelativeError * ActualQuantile andalso
        SampleSize =:= Module:total(EstimatedSketch).

estimate_sketch(Module, Sample, RelativeError, Bound) ->
    Opts = #{error => RelativeError, bound => Bound},
    Sketch = Module:new(Opts),
    lists:foldl(fun(V, S) -> Module:insert(S, V) end, Sketch, Sample).

calculate_sketch(_, Sample, _, _) ->
    Sketch = ddskerl_exact:new(),
    lists:foldl(fun(V, S) -> ddskerl_exact:insert(S, V) end, Sketch, Sample).

pos_real() ->
    union([pos_integer(), float(0.0000001, inf)]).

relative_error() ->
    float(0.001, 0.999999).

bucket_size(Max, Err) ->
    ceil(math:log2(Max) * (1.0 / math:log2((1 + Err) / (1 - Err)))).

%% Non emty list of arbitrarily big positive integers and floats
sample(Module) when ddskerl_std =:= Module; ddskerl_bound =:= Module ->
    non_empty(list(pos_real())).

quantiles() ->
    [0.0, 0.5, 0.60, 0.75, 0.85, 0.90, 0.95, 0.96, 0.97, 0.98, 0.99, 0.999, 1.0].
