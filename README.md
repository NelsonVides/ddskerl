[![Hex.pm](https://img.shields.io/hexpm/v/ddskerl.svg)](https://hex.pm/packages/ddskerl)
[![Hex.pm](https://img.shields.io/hexpm/dt/ddskerl.svg)](https://hex.pm/packages/ddskerl)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/ddskerl/)
[![GitHub Actions](https://github.com/NelsonVides/ddskerl/actions/workflows/main.yml/badge.svg)](https://github.com/NelsonVides/ddskerl/actions/workflows/main.yml)
[![Codecov](https://codecov.io/github/NelsonVides/ddskerl/graph/badge.svg?token=G9HB5UKNIY)](https://codecov.io/github/NelsonVides/ddskerl)

These implementations are all based on the paper
["DDSketch: A Fast and Fully-Mergeable Quantile Sketch with Relative-Error Guarantees"](https://arxiv.org/pdf/1908.10693)
by Charles Masson, Jee E. Rim, Homin K. Lee, and Minos N. Garofalakis.

## Implementations

There are several variations of the implementation, that is:
- `m:ddskerl_std`
- `m:ddskerl_bound`
- `m:ddskerl_counters`
- `m:ddskerl_ets`

They differ in their implementation details and their advantages and disadvantages.
For details, see the documentation for each module.

Generally, `m:ddskerl_std` is the most correct and simplest to use, but also the slowest
(it uses a single map that cannot be shared) and most memory consuming (`log(N)` on the number of
_different_ elements, which might actually suffice for your purposes). `m:ddskerl_counters` is the
fastest and most paralellisable, by an order of magnitude, but it requires setting the number of
buckets based on the expected highest value (which if measuring picoseconds the largest signed
64-bits integer would allow you to measure ~107 days, which might actually suffice too).

## API

For all merge operations, merges are only defined if the sketches are of the same implementation,
are defined with the same error (and therefore the same gamma), and in the case of bucketed
solutions (counters and ets), the same bucket count.
- `ddskerl_std:merge/2`,
- `ddskerl_bound:merge/2`,
- `ddskerl_counters:merge/2`,
- `ddskerl_std:merge/2`,

For all quantile operations, accepted quantiles go from `0.0` (the minimum), to `1.0` (the maximum).
- `ddskerl_std:quantile/2`,
- `ddskerl_bound:quantile/2`,
- `ddskerl_counters:quantile/2`,
- `ddskerl_std:quantile/2`,
