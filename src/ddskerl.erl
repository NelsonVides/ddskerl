-module(ddskerl).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.
?MODULEDOC("""
DDSketch behaviour.
""").

-if(?OTP_RELEASE >= 26).
?DOC("Options for the DDSketch.").
-type opts() :: #{atom() := dynamic()}.
?DOC("DDSketch instance.").
-type ddsketch() :: dynamic().
-else.
?DOC("Options for the DDSketch.").
-type opts() :: #{atom() := _}.
?DOC("DDSketch instance.").
-type ddsketch() :: _.
-endif.

-export_type([opts/0, ddsketch/0]).

?DOC("Create a new DDSketch instance.").
-callback new(opts()) -> ddsketch().

?DOC("Get the total number of elements in the DDSketch.").
-callback total(ddsketch()) -> non_neg_integer().

?DOC("Get the sum number of elements in the DDSketch.").
-callback sum(ddsketch()) -> number().

?DOC("Insert a value into the DDSketch.").
-callback insert(ddsketch(), number()) -> ddsketch().

?DOC("Calculate the quantile of a DDSketch.").
-callback quantile(ddsketch(), float()) -> float() | undefined.

?DOC("Merge two DDSketch instances.").
-callback merge(ddsketch(), ddsketch()) -> ddsketch().
