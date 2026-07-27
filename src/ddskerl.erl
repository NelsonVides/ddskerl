-module(ddskerl).
-moduledoc """
DDSketch behaviour.
""".

-doc "Options for the DDSketch.".
-type opts() :: #{atom() => dynamic()}.

-doc "DDSketch instance.".
-type ddsketch() ::
    ddskerl_std:ddsketch()
    | ddskerl_bound:ddsketch()
    | ddskerl_ets:ddsketch()
    | ddskerl_counters:ddsketch().

-export_type([opts/0, ddsketch/0]).

-doc "Create a new DDSketch instance.".
-callback new(opts()) -> ddsketch().

-doc "Get the total number of elements in the DDSketch.".
-callback total(ddsketch()) -> non_neg_integer().

-doc "Get the sum number of elements in the DDSketch.".
-callback sum(ddsketch()) -> number().

-doc "Insert a value into the DDSketch.".
-callback insert(ddsketch(), number()) -> ddsketch().

-doc "Calculate the quantile of a DDSketch.".
-callback quantile(ddsketch(), float()) -> float() | undefined.

-doc "Merge two DDSketch instances.".
-callback merge(ddsketch(), ddsketch()) -> ddsketch().
