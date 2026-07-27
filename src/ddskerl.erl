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

-doc """
Index identifying a bucket within a DDSketch.

A regular bucket is identified by an integer `I` and covers the values
in the interval `(gamma^(I-1), gamma^I]`. Values that cannot be indexed
that way are accumulated in the following special buckets:
- `zero`, for values of exactly zero, as `log(0)` is undefined;
- `underflow`, for values too small for the smallest allocated bucket;
- `overflow`, for values too large for the largest allocated bucket.

Which of the special buckets a given implementation uses depends on whether
its buckets are preallocated, see `t:bucket/0`.
""".
-type bucket_index() :: integer() | zero | underflow | overflow.

-doc """
A single non-empty bucket of a DDSketch.

`count` values were inserted into the bucket, and all of them fall within
`(lower, upper]`, with the exception of the `zero` bucket, for which both
`lower` and `upper` are `0.0`.

Note that the bounds are the ones the bucket is defined by, not the ones of the
values actually inserted: for the smallest bucket of a saturated `m:ddskerl_bound`
sketch, which collapses buckets on demand, `lower` is only a lower estimate.
""".
-type bucket() :: #{
    index := bucket_index(),
    lower := float(),
    upper := float() | infinity,
    count := pos_integer()
}.

-export_type([opts/0, ddsketch/0, bucket_index/0, bucket/0]).

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

-doc """
Fold over all non-empty buckets of a DDSketch, in ascending order of value.

Empty buckets are never given to the folding function,
hence preallocated implementations only export the buckets they actually used.
""".
-callback foldl(ddsketch(), Fun, Acc0) -> Acc1 when
    Fun :: fun((Bucket :: bucket(), AccIn :: term()) -> AccOut :: term()),
    Acc0 :: term(),
    Acc1 :: term().

-doc "Export all non-empty buckets of a DDSketch, in ascending order of value.".
-callback buckets(ddsketch()) -> [bucket()].
