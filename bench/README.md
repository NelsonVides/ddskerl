# Results

## Machine in use
```
Operating System: macOS
CPU Information: Apple M3 Pro
Number of Available Cores: 12
Available memory: 36 GB
Elixir 1.18.2
Erlang 27.2.3
JIT enabled: true
```


## Benchmark: `insert_many_uniques`: randomly many unique sample
```
warmup: 2 s
time: 5 s
memory time: 5 s
```

##### With input 100_000, error 1%, buckets 2000 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_counters        185.18        5.40 ms     ±6.98%        5.33 ms        7.66 ms
ddskerl_std             107.11        9.34 ms     ±3.25%        9.21 ms       10.48 ms
ddskerl_bound            93.35       10.71 ms     ±8.57%       10.59 ms       12.97 ms
exact                    47.00       21.28 ms    ±20.62%       20.82 ms       48.57 ms
ddskerl_ets              10.90       91.76 ms     ±3.03%       90.42 ms      101.87 ms

Comparison:
ddskerl_counters        185.18
ddskerl_std             107.11 - 1.73x slower +3.94 ms
ddskerl_bound            93.35 - 1.98x slower +5.31 ms
exact                    47.00 - 3.94x slower +15.88 ms
ddskerl_ets              10.90 - 16.99x slower +86.36 ms

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters         4.94 ms        8.76 ms            926         5.33 ms, 5.37 ms
ddskerl_std              9.00 ms       10.95 ms            5369.23 ms, 9.18 ms, 9.27 ms
ddskerl_bound           10.10 ms       25.82 ms            46710.18 ms, 10.87 ms, 10.91
exact                   16.86 ms       55.52 ms            235                     None
ddskerl_ets             89.08 ms      101.87 ms             55                     None

Memory usage statistics:

Name                Memory usage
ddskerl_counters         3.05 MB
ddskerl_std             36.64 MB - 12.01x memory usage +33.59 MB
ddskerl_bound           39.27 MB - 12.87x memory usage +36.22 MB
exact                   51.92 MB - 17.01x memory usage +48.87 MB
ddskerl_ets             13.02 MB - 4.26x memory usage +9.96 MB

**All measurements for memory usage were the same**
```

##### With input 10_000, error 1%, buckets 2000 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_counters       1901.19        0.53 ms     ±6.59%        0.52 ms        0.61 ms
ddskerl_std            1060.13        0.94 ms     ±7.26%        0.93 ms        1.19 ms
ddskerl_bound           868.28        1.15 ms    ±12.36%        1.11 ms        1.47 ms
exact                   749.41        1.33 ms    ±12.95%        1.29 ms        1.94 ms
ddskerl_ets             106.55        9.39 ms     ±9.93%        9.14 ms       15.64 ms

Comparison:
ddskerl_counters       1901.19
ddskerl_std            1060.13 - 1.79x slower +0.42 ms
ddskerl_bound           868.28 - 2.19x slower +0.63 ms
exact                   749.41 - 2.54x slower +0.81 ms
ddskerl_ets             106.55 - 17.84x slower +8.86 ms

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters         0.48 ms        1.60 ms         9.50 K0.50 ms, 0.52 ms, 0.52 ms
ddskerl_std              0.83 ms        1.95 ms         5.30 K0.92 ms, 0.91 ms, 0.91 ms
ddskerl_bound            1.07 ms        5.64 ms         4.34 K                  1.10 ms
exact                    1.02 ms        3.05 ms         3.75 K                  1.25 ms
ddskerl_ets              8.92 ms       17.89 ms            5339.03 ms, 9.09 ms, 9.08 ms

Memory usage statistics:

Name                Memory usage
ddskerl_counters         0.31 MB
ddskerl_std              3.46 MB - 11.34x memory usage +3.16 MB
ddskerl_bound            4.19 MB - 13.70x memory usage +3.88 MB
exact                    4.06 MB - 13.29x memory usage +3.76 MB
ddskerl_ets              1.34 MB - 4.40x memory usage +1.04 MB

**All measurements for memory usage were the same**
```

##### With input 10_000, error 2%, buckets 700 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_counters       1900.50        0.53 ms     ±7.20%        0.52 ms        0.61 ms
ddskerl_std            1168.60        0.86 ms     ±5.80%        0.85 ms        1.05 ms
ddskerl_bound           978.00        1.02 ms     ±7.28%        1.00 ms        1.25 ms
exact                   746.95        1.34 ms    ±12.02%        1.30 ms        1.91 ms
ddskerl_ets             107.74        9.28 ms     ±4.22%        9.14 ms       11.42 ms

Comparison:
ddskerl_counters       1900.50
ddskerl_std            1168.60 - 1.63x slower +0.33 ms
ddskerl_bound           978.00 - 1.94x slower +0.50 ms
exact                   746.95 - 2.54x slower +0.81 ms
ddskerl_ets             107.74 - 17.64x slower +8.76 ms

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters         0.47 ms        1.53 ms         9.50 K                  0.51 ms
ddskerl_std              0.76 ms        1.64 ms         5.84 K0.84 ms, 0.84 ms, 0.84 ms
ddskerl_bound            0.95 ms        2.39 ms         4.89 K                  0.97 ms
exact                    1.06 ms        3.09 ms         3.73 K                  1.27 ms
ddskerl_ets              8.93 ms       11.89 ms            5399.06 ms, 9.10 ms, 9.14 ms

Memory usage statistics:

Name                Memory usage
ddskerl_counters         0.31 MB
ddskerl_std              3.15 MB - 10.31x memory usage +2.84 MB
ddskerl_bound            3.80 MB - 12.42x memory usage +3.49 MB
exact                    4.07 MB - 13.32x memory usage +3.77 MB
ddskerl_ets              1.34 MB - 4.40x memory usage +1.04 MB

**All measurements for memory usage were the same**
```

##### With input 10_000_000, error 1%, buckets 2000 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_counters          1.87         0.54 s     ±3.18%         0.54 s         0.55 s
ddskerl_bound             0.86         1.16 s     ±2.43%         1.16 s         1.19 s
ddskerl_std               0.81         1.23 s     ±2.79%         1.23 s         1.28 s
exact                    0.148         6.77 s     ±0.00%         6.77 s         6.77 s
ddskerl_ets              0.108         9.24 s     ±0.00%         9.24 s         9.24 s

Comparison:
ddskerl_counters          1.87
ddskerl_bound             0.86 - 2.16x slower +0.62 s
ddskerl_std               0.81 - 2.30x slower +0.70 s
exact                    0.148 - 12.64x slower +6.23 s
ddskerl_ets              0.108 - 17.26x slower +8.71 s

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters          0.50 s         0.55 s             10                     None
ddskerl_bound             1.12 s         1.19 s              5                     None
ddskerl_std               1.20 s         1.28 s              5                     None
exact                     6.77 s         6.77 s              1                     None
ddskerl_ets               9.24 s         9.24 s              1                     None

Memory usage statistics:

Name                Memory usage
ddskerl_counters         0.30 GB
ddskerl_bound            3.95 GB - 13.26x memory usage +3.66 GB
ddskerl_std              3.87 GB - 12.99x memory usage +3.57 GB
exact                    7.20 GB - 24.15x memory usage +6.90 GB
ddskerl_ets              1.27 GB - 4.25x memory usage +0.97 GB

**All measurements for memory usage were the same**
```

##### With input 1_000_000, error 1%, buckets 2000 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_counters         18.75       53.34 ms     ±4.23%       52.38 ms       63.90 ms
ddskerl_std              10.63       94.06 ms     ±8.60%       90.06 ms      118.59 ms
ddskerl_bound             8.98      111.31 ms     ±7.41%      106.99 ms      138.28 ms
exact                     2.61      383.03 ms     ±7.41%      375.36 ms      427.54 ms
ddskerl_ets               1.08      923.28 ms     ±1.44%      917.62 ms      945.57 ms

Comparison:
ddskerl_counters         18.75
ddskerl_std              10.63 - 1.76x slower +40.72 ms
ddskerl_bound             8.98 - 2.09x slower +57.97 ms
exact                     2.61 - 7.18x slower +329.69 ms
ddskerl_ets               1.08 - 17.31x slower +869.94 ms

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters        50.73 ms       63.90 ms             94                     None
ddskerl_std             88.35 ms      118.59 ms             54                     None
ddskerl_bound          105.38 ms      138.28 ms             45                     None
exact                  338.71 ms      427.54 ms             14                     None
ddskerl_ets            911.49 ms      945.57 ms              6                     None

Memory usage statistics:

Name                Memory usage
ddskerl_counters        30.52 MB
ddskerl_std            382.52 MB - 12.53x memory usage +352.01 MB
ddskerl_bound          408.83 MB - 13.40x memory usage +378.31 MB
exact                  627.99 MB - 20.58x memory usage +597.47 MB
ddskerl_ets            129.74 MB - 4.25x memory usage +99.23 MB

**All measurements for memory usage were the same**
```


## Benchmark: `insert_sequence`: an ordered range of integers from 1 to bound
```
warmup: 2 s
time: 5 s
memory time: 5 s
```

##### With input 100_000, error 1%, buckets 1000 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_counters        182.70        5.47 ms     ±4.14%        5.40 ms        6.51 ms
ddskerl_std             157.05        6.37 ms     ±6.41%        6.29 ms        7.69 ms
ddskerl_bound           100.71        9.93 ms     ±4.64%        9.74 ms       11.78 ms
exact                    42.55       23.50 ms    ±23.13%       22.47 ms       57.38 ms
ddskerl_ets              16.03       62.39 ms     ±7.03%       61.47 ms       90.23 ms

Comparison:
ddskerl_counters        182.70
ddskerl_std             157.05 - 1.16x slower +0.89 ms
ddskerl_bound           100.71 - 1.81x slower +4.46 ms
exact                    42.55 - 4.29x slower +18.03 ms
ddskerl_ets              16.03 - 11.40x slower +56.91 ms

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters         5.30 ms        8.60 ms            914                  5.38 ms
ddskerl_std              5.74 ms        9.10 ms            7866.86 ms, 6.49 ms, 6.22 ms
ddskerl_bound            9.31 ms       12.66 ms            504         9.76 ms, 9.72 ms
exact                   19.44 ms       58.96 ms            213                 22.34 ms
ddskerl_ets             59.28 ms       90.23 ms             81                     None

Memory usage statistics:

Name                     average  deviation         median         99th %
ddskerl_counters         3.05 MB     ±0.00%        3.05 MB        3.05 MB
ddskerl_std             27.28 MB     ±0.00%       27.28 MB       27.28 MB
ddskerl_bound           46.37 MB     ±0.00%       46.37 MB       46.37 MB
exact                   51.71 MB     ±0.00%       51.71 MB       51.71 MB
ddskerl_ets             16.80 MB     ±0.00%       16.80 MB       16.80 MB

Comparison:
ddskerl_counters         3.05 MB
ddskerl_std             27.28 MB - 8.94x memory usage +24.23 MB
ddskerl_bound           46.37 MB - 15.19x memory usage +43.31 MB
exact                   51.71 MB - 16.94x memory usage +48.66 MB
ddskerl_ets             16.80 MB - 5.50x memory usage +13.75 MB

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters         3.05 MB        3.05 MB            494                  3.05 MB
ddskerl_std             27.28 MB       27.28 MB            258                 27.28 MB
ddskerl_bound           46.37 MB       46.37 MB            143                 46.37 MB
exact                   51.71 MB       51.71 MB            173                 51.71 MB
ddskerl_ets             16.80 MB       16.80 MB             75                 16.80 MB
```

##### With input 100_000, error 1%, buckets 2000 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_counters        169.61        5.90 ms    ±18.16%        5.44 ms        7.74 ms
ddskerl_std             156.58        6.39 ms     ±6.80%        6.29 ms        8.03 ms
ddskerl_bound           100.61        9.94 ms     ±4.87%        9.75 ms       11.94 ms
exact                    42.82       23.35 ms    ±19.24%       22.40 ms       48.49 ms
ddskerl_ets              10.47       95.54 ms     ±1.97%       95.18 ms      101.49 ms

Comparison:
ddskerl_counters        169.61
ddskerl_std             156.58 - 1.08x slower +0.49 ms
ddskerl_bound           100.61 - 1.69x slower +4.04 ms
exact                    42.82 - 3.96x slower +17.45 ms
ddskerl_ets              10.47 - 16.20x slower +89.64 ms

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters         5.30 ms       21.40 ms            8495.37 ms, 5.41 ms, 5.37 ms
ddskerl_std              5.81 ms        8.90 ms            7836.42 ms, 6.85 ms, 6.27 ms
ddskerl_bound            9.32 ms       13.23 ms            5039.70 ms, 9.55 ms, 9.70 ms
exact                   18.41 ms       55.18 ms            215                 22.00 ms
ddskerl_ets             92.92 ms      101.49 ms             53                     None

Memory usage statistics:

Name                     average  deviation         median         99th %
ddskerl_counters         3.05 MB     ±0.00%        3.05 MB        3.05 MB
ddskerl_std             27.28 MB     ±0.00%       27.28 MB       27.28 MB
ddskerl_bound           46.37 MB     ±0.00%       46.37 MB       46.37 MB
exact                   51.71 MB     ±0.01%       51.71 MB       51.72 MB
ddskerl_ets             16.82 MB     ±0.00%       16.82 MB       16.82 MB

Comparison:
ddskerl_counters         3.05 MB
ddskerl_std             27.28 MB - 8.94x memory usage +24.23 MB
ddskerl_bound           46.37 MB - 15.19x memory usage +43.31 MB
exact                   51.71 MB - 16.94x memory usage +48.66 MB
ddskerl_ets             16.82 MB - 5.51x memory usage +13.76 MB

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters         3.05 MB        3.05 MB            482                  3.05 MB
ddskerl_std             27.28 MB       27.28 MB            258                 27.28 MB
ddskerl_bound           46.37 MB       46.37 MB            144                 46.37 MB
exact                   51.71 MB       51.75 MB            181                 51.71 MB
ddskerl_ets             16.82 MB       16.82 MB             48                 16.82 MB
```

##### With input 10_000, error 1%, buckets 1000 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_std             1.64 K      608.51 μs     ±5.83%      601.70 μs      708.20 μs
ddskerl_counters        1.60 K      624.86 μs    ±22.87%      561.08 μs      846.49 μs
ddskerl_bound           1.07 K      934.59 μs     ±6.15%      922.37 μs     1138.76 μs
exact                   0.68 K     1473.18 μs    ±14.44%     1405.59 μs     2191.63 μs
ddskerl_ets            0.163 K     6145.85 μs     ±4.28%     6082.28 μs     7445.80 μs

Comparison:
ddskerl_std             1.64 K
ddskerl_counters        1.60 K - 1.03x slower +16.35 μs
ddskerl_bound           1.07 K - 1.54x slower +326.08 μs
exact                   0.68 K - 2.42x slower +864.67 μs
ddskerl_ets            0.163 K - 10.10x slower +5537.35 μs

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_std            554.33 μs     1178.07 μs         8.21 K                605.74 μs
ddskerl_counters       528.62 μs     2543.48 μs         8.00 K                530.12 μs
ddskerl_bound          878.08 μs     1878.48 μs         5.35 K905.24 μs, 905.87 μs, 965
exact                 1158.24 μs     3369.55 μs         3.39 K   1291.36 μs, 1335.49 μs
ddskerl_ets           5869.07 μs     8615.58 μs            814               6085.57 μs

Memory usage statistics:

Name                     average  deviation         median         99th %
ddskerl_std              2.63 MB     ±0.00%        2.63 MB        2.63 MB
ddskerl_counters         0.31 MB     ±0.00%        0.31 MB        0.31 MB
ddskerl_bound            4.41 MB     ±0.00%        4.41 MB        4.41 MB
exact                    4.03 MB     ±0.00%        4.03 MB        4.03 MB
ddskerl_ets              1.69 MB     ±0.00%        1.69 MB        1.69 MB

Comparison:
ddskerl_std              2.63 MB
ddskerl_counters         0.31 MB - 0.12x memory usage -2.32156 MB
ddskerl_bound            4.41 MB - 1.68x memory usage +1.78 MB
exact                    4.03 MB - 1.53x memory usage +1.40 MB
ddskerl_ets              1.69 MB - 0.64x memory usage -0.93311 MB

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_std              2.63 MB        2.63 MB         2.54 K                  2.63 MB
ddskerl_counters         0.31 MB        0.31 MB         4.65 K                  0.31 MB
ddskerl_bound            4.41 MB        4.41 MB         1.36 K                  4.41 MB
exact                    4.03 MB        4.03 MB         2.20 K                  4.03 MB
ddskerl_ets              1.69 MB        1.69 MB            771                  1.69 MB
```

##### With input 10_000, error 1%, buckets 2000 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_std             1.66 K      603.02 μs     ±7.12%      595.74 μs      706.75 μs
ddskerl_counters        1.65 K      604.55 μs    ±19.88%      568.33 μs      835.37 μs
ddskerl_bound           1.07 K      935.91 μs     ±6.23%      923.99 μs     1118.77 μs
exact                   0.68 K     1471.67 μs    ±14.21%     1417.53 μs     2108.67 μs
ddskerl_ets            0.105 K     9556.75 μs     ±3.87%     9501.53 μs    11136.84 μs

Comparison:
ddskerl_std             1.66 K
ddskerl_counters        1.65 K - 1.00x slower +1.54 μs
ddskerl_bound           1.07 K - 1.55x slower +332.89 μs
exact                   0.68 K - 2.44x slower +868.65 μs
ddskerl_ets            0.105 K - 15.85x slower +8953.73 μs

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_std            550.20 μs     1548.19 μs         8.29 K                597.12 μs
ddskerl_counters       517.70 μs     3252.59 μs         8.27 K     556.75 μs, 547.91 μs
ddskerl_bound          836.74 μs     1917.40 μs         5.34 K                903.03 μs
exact                 1173.78 μs     3200.30 μs         3.40 K   1613.15 μs, 1294.36 μs
ddskerl_ets           9069.46 μs    13682.74 μs            524               9460.33 μs

Memory usage statistics:

Name                     average  deviation         median         99th %
ddskerl_std              2.63 MB     ±0.00%        2.63 MB        2.63 MB
ddskerl_counters         0.31 MB     ±0.00%        0.31 MB        0.31 MB
ddskerl_bound            4.41 MB     ±0.00%        4.41 MB        4.41 MB
exact                    4.03 MB     ±0.00%        4.03 MB        4.03 MB
ddskerl_ets              1.71 MB     ±0.00%        1.71 MB        1.71 MB

Comparison:
ddskerl_std              2.63 MB
ddskerl_counters         0.31 MB - 0.12x memory usage -2.32156 MB
ddskerl_bound            4.41 MB - 1.68x memory usage +1.78 MB
exact                    4.03 MB - 1.53x memory usage +1.40 MB
ddskerl_ets              1.71 MB - 0.65x memory usage -0.91788 MB

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_std              2.63 MB        2.63 MB         2.55 K                  2.63 MB
ddskerl_counters         0.31 MB        0.31 MB         4.64 K                  0.31 MB
ddskerl_bound            4.41 MB        4.41 MB         1.37 K                  4.41 MB
exact                    4.03 MB        4.03 MB         2.20 K                  4.03 MB
ddskerl_ets              1.71 MB        1.71 MB            489                  1.71 MB
```

##### With input 1_000_000, error 1%, buckets 1000 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_counters         18.13       55.17 ms     ±2.52%       54.70 ms       60.64 ms
ddskerl_std              13.61       73.46 ms     ±8.85%       71.67 ms      108.94 ms
ddskerl_bound             9.90      101.00 ms     ±2.89%      100.75 ms      109.70 ms
exact                     2.74      365.41 ms     ±6.13%      357.09 ms      409.64 ms
ddskerl_ets               1.63      613.38 ms     ±1.34%      613.78 ms      624.37 ms

Comparison:
ddskerl_counters         18.13
ddskerl_std              13.61 - 1.33x slower +18.29 ms
ddskerl_bound             9.90 - 1.83x slower +45.83 ms
exact                     2.74 - 6.62x slower +310.24 ms
ddskerl_ets               1.63 - 11.12x slower +558.21 ms

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters        53.71 ms       60.64 ms             91                     None
ddskerl_std             69.26 ms      108.94 ms             69                     None
ddskerl_bound           96.40 ms      109.70 ms             50                     None
exact                  337.64 ms      409.64 ms             14                     None
ddskerl_ets            603.66 ms      624.37 ms              9                     None

Memory usage statistics:

Name                     average  deviation         median         99th %
ddskerl_counters        30.52 MB     ±0.00%       30.52 MB       30.52 MB
ddskerl_std            275.10 MB     ±0.00%      275.10 MB      275.11 MB
ddskerl_bound          462.79 MB     ±0.00%      462.79 MB      462.79 MB
exact                  627.20 MB     ±0.00%      627.20 MB      627.20 MB
ddskerl_ets            167.86 MB     ±0.00%      167.86 MB      167.86 MB

Comparison:
ddskerl_counters        30.52 MB
ddskerl_std            275.10 MB - 9.01x memory usage +244.58 MB
ddskerl_bound          462.79 MB - 15.16x memory usage +432.27 MB
exact                  627.20 MB - 20.55x memory usage +596.68 MB
ddskerl_ets            167.86 MB - 5.50x memory usage +137.34 MB

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters        30.52 MB       30.52 MB             48                 30.52 MB
ddskerl_std            275.10 MB      275.11 MB             24                275.10 MB
ddskerl_bound          462.79 MB      462.79 MB             17                462.79 MB
exact                  627.20 MB      627.20 MB             12                627.20 MB
ddskerl_ets            167.86 MB      167.86 MB              8                167.86 MB
```

##### With input 1_000_000, error 1%, buckets 2000 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_counters         15.92       62.82 ms    ±18.99%       55.50 ms      107.65 ms
ddskerl_std              13.70       72.97 ms     ±6.87%       71.40 ms      100.61 ms
ddskerl_bound             9.89      101.13 ms     ±2.23%      100.80 ms      108.23 ms
exact                     2.76      362.88 ms     ±5.93%      361.70 ms      414.97 ms
ddskerl_ets               1.04      958.94 ms     ±0.61%      958.41 ms      968.54 ms

Comparison:
ddskerl_counters         15.92
ddskerl_std              13.70 - 1.16x slower +10.15 ms
ddskerl_bound             9.89 - 1.61x slower +38.32 ms
exact                     2.76 - 5.78x slower +300.06 ms
ddskerl_ets               1.04 - 15.27x slower +896.12 ms

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters        54.09 ms      107.65 ms             80                     None
ddskerl_std             69.53 ms      100.61 ms             69                     None
ddskerl_bound           97.64 ms      108.23 ms             50                     None
exact                  337.65 ms      414.97 ms             14                     None
ddskerl_ets            952.34 ms      968.54 ms              6                     None

Memory usage statistics:

Name                     average  deviation         median         99th %
ddskerl_counters        30.52 MB     ±0.00%       30.52 MB       30.52 MB
ddskerl_std            275.10 MB     ±0.00%      275.10 MB      275.11 MB
ddskerl_bound          462.79 MB     ±0.00%      462.79 MB      462.79 MB
exact                  627.20 MB     ±0.00%      627.20 MB      627.20 MB
ddskerl_ets            167.88 MB     ±0.00%      167.88 MB      167.88 MB

Comparison:
ddskerl_counters        30.52 MB
ddskerl_std            275.10 MB - 9.01x memory usage +244.58 MB
ddskerl_bound          462.79 MB - 15.16x memory usage +432.27 MB
exact                  627.20 MB - 20.55x memory usage +596.68 MB
ddskerl_ets            167.88 MB - 5.50x memory usage +137.36 MB

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters        30.52 MB       30.52 MB             48                 30.52 MB
ddskerl_std            275.10 MB      275.11 MB             25                275.10 MB
ddskerl_bound          462.79 MB      462.79 MB             18                462.79 MB
exact                  627.20 MB      627.20 MB             12                627.20 MB
ddskerl_ets            167.88 MB      167.88 MB              5                167.88 MB
```


## Benchmark: `insert_one_new_over_a_full_sketch`: new random element in a random sample
```
warmup: 2 s
time: 5 s
memory time: 5 s
reduction time: 5 s
```

##### With input 100_000, error 1% #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_ets          916605.56     0.00109 ms    ±22.83%     0.00096 ms     0.00183 ms
ddskerl_counters        180.98        5.53 ms     ±9.97%        5.39 ms        9.97 ms
ddskerl_std             106.49        9.39 ms     ±2.64%        9.33 ms       10.24 ms
ddskerl_bound            88.85       11.25 ms     ±2.04%       11.17 ms       11.97 ms
exact                    47.21       21.18 ms     ±7.04%       20.83 ms       24.91 ms

Comparison:
ddskerl_ets          916605.56
ddskerl_counters        180.98 - 5064.62x slower +5.52 ms
ddskerl_std             106.49 - 8607.22x slower +9.39 ms
ddskerl_bound            88.85 - 10315.78x slower +11.25 ms
exact                    47.21 - 19416.35x slower +21.18 ms

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_ets           0.00088 ms     0.00183 ms             55               0.00092 ms
ddskerl_counters         5.28 ms        9.97 ms             91                     None
ddskerl_std              9.09 ms       10.24 ms             54                     None
ddskerl_bound           11.02 ms       11.97 ms             45                     None
exact                   19.11 ms       24.91 ms             24                     None

Memory usage statistics:

Name                Memory usage
ddskerl_ets                136 B
ddskerl_counters            32 B - 0.24x memory usage -104 B
ddskerl_std                 96 B - 0.71x memory usage -40 B
ddskerl_bound              448 B - 3.29x memory usage +312 B
exact                      656 B - 4.82x memory usage +520 B

**All measurements for memory usage were the same**

Reduction count statistics:

Name             Reduction count
ddskerl_ets                   12
ddskerl_counters              14 - 1.17x reduction count +2
ddskerl_std                    7 - 0.58x reduction count -5
ddskerl_bound                 43 - 3.58x reduction count +31
exact                          4 - 0.33x reduction count -8

**All measurements for reduction count were the same**
```

##### With input 10_000, error 1% #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_ets          1048.20 K     0.00095 ms    ±13.54%     0.00092 ms     0.00159 ms
ddskerl_counters        1.87 K        0.54 ms     ±7.90%        0.53 ms        0.67 ms
ddskerl_std             1.01 K        0.99 ms     ±9.18%        0.97 ms        1.30 ms
ddskerl_bound           0.84 K        1.19 ms     ±8.96%        1.18 ms        1.40 ms
exact                   0.69 K        1.44 ms    ±12.37%        1.41 ms        2.27 ms

Comparison:
ddskerl_ets          1048.20 K
ddskerl_counters        1.87 K - 561.71x slower +0.53 ms
ddskerl_std             1.01 K - 1038.07x slower +0.99 ms
ddskerl_bound           0.84 K - 1249.79x slower +1.19 ms
exact                   0.69 K - 1510.68x slower +1.44 ms

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_ets           0.00083 ms     0.00196 ms            543               0.00092 ms
ddskerl_counters         0.51 ms        1.28 ms            933                  0.54 ms
ddskerl_std              0.91 ms        2.47 ms            5050.98 ms, 0.96 ms, 0.96 ms
ddskerl_bound            1.09 ms        3.22 ms            4201.18 ms, 1.18 ms, 1.19 ms
exact                    1.26 ms        3.43 ms            347                     None

Memory usage statistics:

Name                Memory usage
ddskerl_ets                136 B
ddskerl_counters            32 B - 0.24x memory usage -104 B
ddskerl_std                384 B - 2.82x memory usage +248 B
ddskerl_bound              408 B - 3.00x memory usage +272 B
exact                      456 B - 3.35x memory usage +320 B

**All measurements for memory usage were the same**

Reduction count statistics:

Name             Reduction count
ddskerl_ets                   12
ddskerl_counters              14 - 1.17x reduction count +2
ddskerl_std                    7 - 0.58x reduction count -5
ddskerl_bound                 39 - 3.25x reduction count +27
exact                          4 - 0.33x reduction count -8

**All measurements for reduction count were the same**
```

##### With input 1_000_000, error 1% #####
```
Name                       ips        average  deviation         median         99th %
exact               1195763.58     0.00084 ms    ±45.09%     0.00088 ms     0.00183 ms
ddskerl_ets          742206.83     0.00135 ms     ±8.45%     0.00135 ms     0.00150 ms
ddskerl_counters         18.65       53.61 ms     ±1.43%       53.62 ms       54.83 ms
ddskerl_std              10.22       97.80 ms     ±2.14%       97.29 ms      101.90 ms
ddskerl_bound             8.69      115.10 ms     ±1.15%      114.81 ms      117.04 ms

Comparison:
exact               1195763.58
ddskerl_ets          742206.83 - 1.61x slower +0.00051 ms
ddskerl_counters         18.65 - 64108.94x slower +53.61 ms
ddskerl_std              10.22 - 116946.29x slower +97.80 ms
ddskerl_bound             8.69 - 137630.96x slower +115.10 ms

Extended statistics:

Name                     minimum        maximum    sample size                     mode
exact                 0.00021 ms     0.00183 ms             14               0.00088 ms
ddskerl_ets           0.00121 ms     0.00150 ms              6               0.00142 ms
ddskerl_counters        52.57 ms       54.83 ms             10                     None
ddskerl_std             96.05 ms      101.90 ms              6                     None
ddskerl_bound          113.48 ms      117.04 ms              5                     None

Memory usage statistics:

Name                Memory usage
exact                      728 B
ddskerl_ets                136 B - 0.19x memory usage -592 B
ddskerl_counters            32 B - 0.04x memory usage -696 B
ddskerl_std                432 B - 0.59x memory usage -296 B
ddskerl_bound              488 B - 0.67x memory usage -240 B

**All measurements for memory usage were the same**

Reduction count statistics:

Name             Reduction count
exact                          4
ddskerl_ets                   12 - 3.00x reduction count +8
ddskerl_counters              14 - 3.50x reduction count +10
ddskerl_std                    7 - 1.75x reduction count +3
ddskerl_bound                 47 - 11.75x reduction count +43
```


## Benchmark: `query_quantiles_over_sequential_input`: an ordered range of integers from 1 to bound
```
warmup: 2 s
time: 5 s
memory time: 5 s
reduction time: 5 s
```

##### With input 100_000, error 1%, buckets 2000, quantile 0.5 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_bound         655.92 K        1.52 μs    ±74.50%        1.21 μs        6.43 μs
ddskerl_counters      652.19 K        1.53 μs    ±63.52%        1.17 μs        6.36 μs
ddskerl_std           275.85 K        3.63 μs    ±67.63%        3.13 μs       14.29 μs
ddskerl_ets           125.83 K        7.95 μs    ±37.33%        8.56 μs       17.54 μs
exact                 0.0531 K    18820.01 μs    ±13.81%    18254.66 μs    32476.36 μs

Comparison:
ddskerl_bound         655.92 K
ddskerl_counters      652.19 K - 1.01x slower +0.00871 μs
ddskerl_std           275.85 K - 2.38x slower +2.10 μs
ddskerl_ets           125.83 K - 5.21x slower +6.42 μs
exact                 0.0531 K - 12344.38x slower +18818.49 μs

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_bound            0.75 μs        9.08 μs            464                  0.83 μs
ddskerl_counters         1.08 μs        8.79 μs            912                  1.13 μs
ddskerl_std              1.79 μs       22.38 μs            765                  1.83 μs
ddskerl_ets              2.54 μs       17.54 μs             529.63 μs, 4.75 μs, 10.88 μ
exact                15985.13 μs    32527.63 μs            127              17670.41 μs

Memory usage statistics:

Name                Memory usage
ddskerl_bound            3.62 KB
ddskerl_counters       0.0469 KB - 0.01x memory usage -3.57031 KB
ddskerl_std              6.48 KB - 1.79x memory usage +2.86 KB
ddskerl_ets            0.0469 KB - 0.01x memory usage -3.57031 KB
exact                29516.30 KB - 8160.01x memory usage +29512.68 KB

**All measurements for memory usage were the same**

Reduction count statistics:

Name                     average  deviation         median         99th %
ddskerl_bound                341     ±0.00%            341            341
ddskerl_counters             226     ±0.00%            226            226
ddskerl_std                 1272     ±0.00%           1272           1272
ddskerl_ets                  261     ±0.00%            261            261
exact                 5402452.66     ±0.09%        5401550     5432188.20

Comparison:
ddskerl_bound                341
ddskerl_counters             226 - 0.66x reduction count -115
ddskerl_std                 1272 - 3.73x reduction count +931
ddskerl_ets                  261 - 0.77x reduction count -80
exact                 5402452.66 - 15842.97x reduction count +5402111.66

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_bound                341            341            438                      341
ddskerl_counters             226            226            859                      226
ddskerl_std                 1272           1272            681                     1272
ddskerl_ets                  261            261             53                      261
exact                    5399088        5432416            1165400928, 5400728, 5399560
```

##### With input 100_000, error 1%, buckets 2000, quantile 0.9 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_bound         639.86 K        1.56 μs    ±73.08%        1.21 μs        6.82 μs
ddskerl_counters      636.13 K        1.57 μs    ±66.41%        1.25 μs        6.69 μs
ddskerl_std           278.86 K        3.59 μs    ±74.28%        3.08 μs       14.77 μs
ddskerl_ets           155.71 K        6.42 μs    ±48.66%        5.33 μs       20.17 μs
exact                 0.0528 K    18938.76 μs    ±10.28%    18671.58 μs    31774.20 μs

Comparison:
ddskerl_bound         639.86 K
ddskerl_counters      636.13 K - 1.01x slower +0.00918 μs
ddskerl_std           278.86 K - 2.29x slower +2.02 μs
ddskerl_ets           155.71 K - 4.11x slower +4.86 μs
exact                 0.0528 K - 12118.18x slower +18937.19 μs

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_bound            0.83 μs        7.79 μs            469                  0.88 μs
ddskerl_counters         1.13 μs       12.75 μs            912                  1.25 μs
ddskerl_std              1.83 μs       31.21 μs            779                  1.88 μs
ddskerl_ets              2.42 μs       20.17 μs             525.38 μs, 4.92 μs, 3.54 μs
exact                16453.83 μs    32021.79 μs            127                     None

Memory usage statistics:

Name                Memory usage
ddskerl_bound            3.81 KB
ddskerl_counters       0.0469 KB - 0.01x memory usage -3.76563 KB
ddskerl_std              6.48 KB - 1.70x memory usage +2.66 KB
ddskerl_ets            0.0469 KB - 0.01x memory usage -3.76563 KB
exact                29516.30 KB - 7741.98x memory usage +29512.48 KB

**All measurements for memory usage were the same**

Reduction count statistics:

Name                     average  deviation         median         99th %
ddskerl_bound                356     ±0.00%            356            356
ddskerl_counters             238     ±0.00%            238            238
ddskerl_std                 1281     ±0.00%           1281           1281
ddskerl_ets                  264     ±0.00%            264            264
exact                 5521897.74     ±0.06%        5521320     5543791.04

Comparison:
ddskerl_bound                356
ddskerl_counters             238 - 0.67x reduction count -118
ddskerl_std                 1281 - 3.60x reduction count +925
ddskerl_ets                  264 - 0.74x reduction count -92
exact                 5521897.74 - 15510.95x reduction count +5521541.74

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_bound                356            356            439                      356
ddskerl_counters             238            238            862                      238
ddskerl_std                 1281           1281            689                     1281
ddskerl_ets                  264            264             53                      264
exact                    5519396        5545512            1155523560, 5519704, 5523088
```

##### With input 100_000, error 1%, buckets 2000, quantile 0.95 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_bound         646.72 K        1.55 μs    ±81.91%        1.21 μs        7.56 μs
ddskerl_counters      632.59 K        1.58 μs    ±61.10%        1.25 μs        6.44 μs
ddskerl_std           244.87 K        4.08 μs    ±89.71%        3.21 μs       17.75 μs
ddskerl_ets           148.91 K        6.72 μs    ±48.34%        6.02 μs       18.79 μs
exact                 0.0518 K    19291.80 μs    ±14.26%    18643.16 μs    38076.53 μs

Comparison:
ddskerl_bound         646.72 K
ddskerl_counters      632.59 K - 1.02x slower +0.0345 μs
ddskerl_std           244.87 K - 2.64x slower +2.54 μs
ddskerl_ets           148.91 K - 4.34x slower +5.17 μs
exact                 0.0518 K - 12476.45x slower +19290.25 μs

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_bound            0.83 μs        8.83 μs            487                  0.88 μs
ddskerl_counters         1.17 μs       11.63 μs            912                  1.25 μs
ddskerl_std              1.79 μs       52.75 μs            775                  1.88 μs
ddskerl_ets              2.46 μs       18.79 μs             526.54 μs, 4.38 μs, 6.13 μs
exact                16237.79 μs    39452.78 μs            124                     None

Memory usage statistics:

Name                Memory usage
ddskerl_bound            3.87 KB
ddskerl_counters       0.0469 KB - 0.01x memory usage -3.82031 KB
ddskerl_std              6.48 KB - 1.67x memory usage +2.61 KB
ddskerl_ets            0.0469 KB - 0.01x memory usage -3.82031 KB
exact                29516.30 KB - 7632.50x memory usage +29512.43 KB

**All measurements for memory usage were the same**

Reduction count statistics:

Name                     average  deviation         median         99th %
ddskerl_bound                361     ±0.00%            361            361
ddskerl_counters             242     ±0.00%            242            242
ddskerl_std                 1284     ±0.00%           1284           1284
ddskerl_ets                  265     ±0.00%            265            265
exact                 5537098.05     ±0.06%        5536600     5558328.48

Comparison:
ddskerl_bound                361
ddskerl_counters             242 - 0.67x reduction count -119
ddskerl_std                 1284 - 3.56x reduction count +923
ddskerl_ets                  265 - 0.73x reduction count -96
exact                 5537098.05 - 15338.22x reduction count +5536737.05

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_bound                361            361            441                      361
ddskerl_counters             242            242            861                      242
ddskerl_std                 1284           1284            684                     1284
ddskerl_ets                  265            265             52                      265
exact                    5534296        5559908            1155535556, 5536288, 5537360
```

##### With input 100_000, error 1%, buckets 2000, quantile 0.999 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_bound         665.83 K        1.50 μs    ±81.21%        1.17 μs           7 μs
ddskerl_counters      592.21 K        1.69 μs    ±72.35%        1.29 μs        7.00 μs
ddskerl_std           278.48 K        3.59 μs    ±67.65%        3.13 μs       14.30 μs
ddskerl_ets           169.49 K        5.90 μs    ±39.51%        5.42 μs       11.83 μs
exact                 0.0522 K    19147.98 μs    ±11.90%    18729.87 μs    35537.98 μs

Comparison:
ddskerl_bound         665.83 K
ddskerl_counters      592.21 K - 1.12x slower +0.187 μs
ddskerl_std           278.48 K - 2.39x slower +2.09 μs
ddskerl_ets           169.49 K - 3.93x slower +4.40 μs
exact                 0.0522 K - 12749.33x slower +19146.48 μs

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_bound            0.83 μs       11.17 μs            487                  0.88 μs
ddskerl_counters         1.17 μs       11.67 μs            911                  1.25 μs
ddskerl_std              1.79 μs       24.71 μs            789                  1.88 μs
ddskerl_ets              2.54 μs       11.83 μs             535.42 μs, 4.92 μs, 5.33 μs
exact                16471.58 μs    38022.99 μs            126                     None

Memory usage statistics:

Name                Memory usage
ddskerl_bound            3.87 KB
ddskerl_counters       0.0469 KB - 0.01x memory usage -3.82031 KB
ddskerl_std              6.48 KB - 1.67x memory usage +2.61 KB
ddskerl_ets            0.0469 KB - 0.01x memory usage -3.82031 KB
exact                29516.30 KB - 7632.50x memory usage +29512.43 KB

**All measurements for memory usage were the same**

Reduction count statistics:

Name                     average  deviation         median         99th %
ddskerl_bound                361     ±0.00%            361            361
ddskerl_counters             242     ±0.00%            242            242
ddskerl_std                 1284     ±0.00%           1284           1284
ddskerl_ets                  265     ±0.00%            265            265
exact                 5551985.15     ±0.07%        5551168     5573881.60

Comparison:
ddskerl_bound                361
ddskerl_counters             242 - 0.67x reduction count -119
ddskerl_std                 1284 - 3.56x reduction count +923
ddskerl_ets                  265 - 0.73x reduction count -96
exact                 5551985.15 - 15379.46x reduction count +5551624.15

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_bound                361            361            446                      361
ddskerl_counters             242            242            858                      242
ddskerl_std                 1284           1284            679                     1284
ddskerl_ets                  265            265             53                      265
exact                    5549008        5574512            1155551300, 5549764, 5551200
```

##### With input 100_000, error 1%, buckets 500, quantile 0.5 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_counters      656.47 K        1.52 μs    ±68.63%        1.21 μs        6.42 μs
ddskerl_bound         618.91 K        1.62 μs   ±121.76%        1.21 μs        7.98 μs
ddskerl_ets           334.86 K        2.99 μs    ±66.84%        2.25 μs        9.35 μs
ddskerl_std           263.01 K        3.80 μs    ±98.27%        3.17 μs       19.24 μs
exact                 0.0536 K    18658.26 μs    ±11.02%    18262.58 μs    31767.62 μs

Comparison:
ddskerl_counters      656.47 K
ddskerl_bound         618.91 K - 1.06x slower +0.0924 μs
ddskerl_ets           334.86 K - 1.96x slower +1.46 μs
ddskerl_std           263.01 K - 2.50x slower +2.28 μs
exact                 0.0536 K - 12248.66x slower +18656.74 μs

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters         1.08 μs       11.63 μs            912                  1.13 μs
ddskerl_bound            0.79 μs       33.96 μs            465                  0.88 μs
ddskerl_ets              1.08 μs        9.38 μs            110                  1.17 μs
ddskerl_std              1.75 μs       44.33 μs            767                  1.83 μs
exact                15981.42 μs    32935.46 μs            128                     None

Memory usage statistics:

Name                Memory usage
ddskerl_counters       0.0469 KB
ddskerl_bound            3.62 KB - 77.17x memory usage +3.57 KB
ddskerl_ets            0.0469 KB - 1.00x memory usage +0 KB
ddskerl_std              6.48 KB - 138.17x memory usage +6.43 KB
exact                29516.30 KB - 629681.00x memory usage +29516.25 KB

**All measurements for memory usage were the same**

Reduction count statistics:

Name                     average  deviation         median         99th %
ddskerl_counters             226     ±0.00%            226            226
ddskerl_bound                341     ±0.00%            341            341
ddskerl_ets                  111     ±0.00%            111            111
ddskerl_std                 1272     ±0.00%           1272           1272
exact                 5401920.07     ±0.05%        5401558     5418120.36

Comparison:
ddskerl_counters             226
ddskerl_bound                341 - 1.51x reduction count +115
ddskerl_ets                  111 - 0.49x reduction count -115
ddskerl_std                 1272 - 5.63x reduction count +1046
exact                 5401920.07 - 23902.30x reduction count +5401694.07

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters             226            226            858                      226
ddskerl_bound                341            341            441                      341
ddskerl_ets                  111            111            110                      111
ddskerl_std                 1272           1272            683                     1272
exact                    5399384        5418680            116                  5400152
```

##### With input 100_000, error 1%, buckets 500, quantile 0.9 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_counters      628.36 K        1.59 μs    ±61.59%        1.25 μs        6.41 μs
ddskerl_bound         586.35 K        1.71 μs    ±83.12%        1.29 μs        7.85 μs
ddskerl_ets           271.41 K        3.68 μs    ±73.74%        2.75 μs       17.67 μs
ddskerl_std           247.64 K        4.04 μs    ±76.04%        3.21 μs       15.21 μs
exact                 0.0528 K    18940.94 μs     ±9.57%    18551.08 μs    29036.25 μs

Comparison:
ddskerl_counters      628.36 K
ddskerl_bound         586.35 K - 1.07x slower +0.114 μs
ddskerl_ets           271.41 K - 2.32x slower +2.09 μs
ddskerl_std           247.64 K - 2.54x slower +2.45 μs
exact                 0.0528 K - 11901.69x slower +18939.35 μs

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters         1.13 μs        9.04 μs            909                  1.25 μs
ddskerl_bound            0.83 μs       12.54 μs            464                  0.88 μs
ddskerl_ets              1.08 μs       18.13 μs            105            2 μs, 3.04 μs
ddskerl_std              1.79 μs       28.29 μs            765                  1.88 μs
exact                16760.83 μs    29093.63 μs            127                     None

Memory usage statistics:

Name                Memory usage
ddskerl_counters       0.0469 KB
ddskerl_bound            3.81 KB - 81.33x memory usage +3.77 KB
ddskerl_ets            0.0469 KB - 1.00x memory usage +0 KB
ddskerl_std              6.48 KB - 138.17x memory usage +6.43 KB
exact                29516.30 KB - 629681.00x memory usage +29516.25 KB

**All measurements for memory usage were the same**

Reduction count statistics:

Name                     average  deviation         median         99th %
ddskerl_counters             238     ±0.00%            238            238
ddskerl_bound                356     ±0.00%            356            356
ddskerl_ets                  114     ±0.00%            114            114
ddskerl_std                 1281     ±0.00%           1281           1281
exact                 5522324.38     ±0.07%        5521716     5544495.52

Comparison:
ddskerl_counters             238
ddskerl_bound                356 - 1.50x reduction count +118
ddskerl_ets                  114 - 0.48x reduction count -124
ddskerl_std                 1281 - 5.38x reduction count +1043
exact                 5522324.38 - 23203.04x reduction count +5522086.38

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters             238            238            846                      238
ddskerl_bound                356            356            429                      356
ddskerl_ets                  114            114            111                      114
ddskerl_std                 1281           1281            684                     1281
exact                    5519112        5544520            1165523064, 5520896, 5523044
```

##### With input 100_000, error 1%, buckets 500, quantile 0.95 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_bound         729.68 K        1.37 μs    ±73.67%        1.13 μs        7.08 μs
ddskerl_counters      590.80 K        1.69 μs    ±81.39%        1.25 μs        6.51 μs
ddskerl_ets           318.54 K        3.14 μs    ±71.51%        2.33 μs        9.49 μs
ddskerl_std           284.50 K        3.51 μs    ±66.86%        3.13 μs       14.75 μs
exact                 0.0522 K    19148.08 μs    ±12.66%    18659.24 μs    35933.37 μs

Comparison:
ddskerl_bound         729.68 K
ddskerl_counters      590.80 K - 1.24x slower +0.32 μs
ddskerl_ets           318.54 K - 2.29x slower +1.77 μs
ddskerl_std           284.50 K - 2.56x slower +2.14 μs
exact                 0.0522 K - 13971.97x slower +19146.71 μs

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_bound            0.83 μs        7.54 μs            489                  0.88 μs
ddskerl_counters         1.13 μs       28.67 μs            910                  1.21 μs
ddskerl_ets              1.13 μs        9.50 μs            110                  1.88 μs
ddskerl_std              1.79 μs       18.08 μs            784                  1.88 μs
exact                16593.33 μs    38383.32 μs            127                     None

Memory usage statistics:

Name                Memory usage
ddskerl_bound            3.87 KB
ddskerl_counters       0.0469 KB - 0.01x memory usage -3.82031 KB
ddskerl_ets            0.0469 KB - 0.01x memory usage -3.82031 KB
ddskerl_std              6.48 KB - 1.67x memory usage +2.61 KB
exact                29516.30 KB - 7632.50x memory usage +29512.43 KB

**All measurements for memory usage were the same**

Reduction count statistics:

Name                     average  deviation         median         99th %
ddskerl_bound                361     ±0.00%            361            361
ddskerl_counters             242     ±0.00%            242            242
ddskerl_ets                  115     ±0.00%            115            115
ddskerl_std                 1284     ±0.00%           1284           1284
exact                 5537003.52     ±0.06%        5536534     5559757.60

Comparison:
ddskerl_bound                361
ddskerl_counters             242 - 0.67x reduction count -119
ddskerl_ets                  115 - 0.32x reduction count -246
ddskerl_std                 1284 - 3.56x reduction count +923
exact                 5537003.52 - 15337.96x reduction count +5536642.52

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_bound                361            361            424                      361
ddskerl_counters             242            242            857                      242
ddskerl_ets                  115            115            108                      115
ddskerl_std                 1284           1284            680                     1284
exact                    5534184        5560832            1165536208, 5536420, 5535736
```

##### With input 100_000, error 1%, buckets 500, quantile 0.999 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_counters      619.22 K        1.61 μs    ±64.28%        1.29 μs        6.13 μs
ddskerl_bound         512.67 K        1.95 μs    ±95.12%        1.25 μs        8.34 μs
ddskerl_ets           325.38 K        3.07 μs    ±62.19%        2.33 μs        9.73 μs
ddskerl_std           277.38 K        3.61 μs    ±72.73%        3.08 μs       15.46 μs
exact                 0.0502 K    19924.10 μs    ±22.41%    18983.70 μs    51151.09 μs

Comparison:
ddskerl_counters      619.22 K
ddskerl_bound         512.67 K - 1.21x slower +0.34 μs
ddskerl_ets           325.38 K - 1.90x slower +1.46 μs
ddskerl_std           277.38 K - 2.23x slower +1.99 μs
exact                 0.0502 K - 12337.31x slower +19922.48 μs

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters         1.13 μs       12.33 μs            911                  1.25 μs
ddskerl_bound            0.83 μs       16.63 μs            476                  0.88 μs
ddskerl_ets              1.13 μs        9.83 μs            109                  1.13 μs
ddskerl_std              1.79 μs       35.12 μs            784                  1.88 μs
exact                17243.00 μs    53273.36 μs            122              18457.08 μs

Memory usage statistics:

Name                Memory usage
ddskerl_counters       0.0469 KB
ddskerl_bound            3.87 KB - 82.50x memory usage +3.82 KB
ddskerl_ets            0.0469 KB - 1.00x memory usage +0 KB
ddskerl_std              6.48 KB - 138.17x memory usage +6.43 KB
exact                29516.30 KB - 629681.00x memory usage +29516.25 KB

**All measurements for memory usage were the same**

Reduction count statistics:

Name                     average  deviation         median         99th %
ddskerl_counters             242     ±0.00%            242            242
ddskerl_bound                361     ±0.00%            361            361
ddskerl_ets                  115     ±0.00%            115            115
ddskerl_std                 1284     ±0.00%           1284           1284
exact                 5551887.66     ±0.07%        5551024     5575378.04

Comparison:
ddskerl_counters             242
ddskerl_bound                361 - 1.49x reduction count +119
ddskerl_ets                  115 - 0.48x reduction count -127
ddskerl_std                 1284 - 5.31x reduction count +1042
exact                 5551887.66 - 22941.68x reduction count +5551645.66

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters             242            242            863                      242
ddskerl_bound                361            361            427                      361
ddskerl_ets                  115            115            111                      115
ddskerl_std                 1284           1284            685                     1284
exact                    5548884        5576532            1165550016, 5549280, 5550520
```

##### With input 10_000, error 1%, buckets 2000, quantile 0.95 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_bound        1091.39 K        0.92 μs    ±75.80%        0.75 μs        4.25 μs
ddskerl_counters      909.88 K        1.10 μs    ±38.39%        1.04 μs        2.27 μs
ddskerl_std           458.10 K        2.18 μs   ±104.13%        1.96 μs       10.17 μs
ddskerl_ets           204.93 K        4.88 μs    ±35.50%        4.21 μs       11.57 μs
exact                   0.70 K     1432.20 μs    ±14.39%     1371.12 μs     2293.26 μs

Comparison:
ddskerl_bound        1091.39 K
ddskerl_counters      909.88 K - 1.20x slower +0.183 μs
ddskerl_std           458.10 K - 2.38x slower +1.27 μs
ddskerl_ets           204.93 K - 5.33x slower +3.96 μs
exact                   0.70 K - 1563.09x slower +1431.28 μs

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_bound            0.67 μs          18 μs         5.29 K                  0.75 μs
ddskerl_counters         0.92 μs       13.04 μs         8.74 K                     1 μs
ddskerl_std              1.42 μs      125.17 μs         8.08 K                  1.50 μs
ddskerl_ets              2.38 μs       16.29 μs            521                     4 μs
exact                 1140.83 μs     3792.82 μs         1.74 K1523.70 μs, 1299.00 μs, 1

Memory usage statistics:

Name                Memory usage
ddskerl_bound            3.02 KB
ddskerl_counters       0.0469 KB - 0.02x memory usage -2.97656 KB
ddskerl_std              4.52 KB - 1.50x memory usage +1.50 KB
ddskerl_ets            0.0469 KB - 0.02x memory usage -2.97656 KB
exact                 2565.74 KB - 848.62x memory usage +2562.72 KB

**All measurements for memory usage were the same**

Reduction count statistics:

Name             Reduction count
ddskerl_bound                261
ddskerl_counters             194 - 0.74x reduction count -67
ddskerl_std                 1020 - 3.91x reduction count +759
ddskerl_ets                  253 - 0.97x reduction count -8
exact                     459646 - 1761.10x reduction count +459385

**All measurements for reduction count were the same**
```

##### With input 10_000, error 1%, buckets 500, quantile 0.95 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_bound        1069.70 K        0.93 μs    ±94.78%        0.79 μs        3.92 μs
ddskerl_counters      890.22 K        1.12 μs    ±45.63%        1.04 μs        3.04 μs
ddskerl_ets           588.27 K        1.70 μs    ±75.95%        1.38 μs        7.58 μs
ddskerl_std           494.71 K        2.02 μs    ±62.71%        1.92 μs        5.41 μs
exact                   0.70 K     1431.46 μs    ±13.22%     1371.41 μs     2279.25 μs

Comparison:
ddskerl_bound        1069.70 K
ddskerl_counters      890.22 K - 1.20x slower +0.188 μs
ddskerl_ets           588.27 K - 1.82x slower +0.77 μs
ddskerl_std           494.71 K - 2.16x slower +1.09 μs
exact                   0.70 K - 1531.23x slower +1430.52 μs

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_bound            0.67 μs          26 μs         5.30 K                  0.75 μs
ddskerl_counters         0.88 μs       15.50 μs         8.99 K                  1.04 μs
ddskerl_ets              0.96 μs       14.08 μs         1.10 K                     1 μs
ddskerl_std              1.42 μs       86.25 μs         8.25 K                  1.50 μs
exact                 1145.50 μs     2998.87 μs         1.74 K1318.70 μs, 1404.50 μs, 1

Memory usage statistics:

Name                Memory usage
ddskerl_bound            3.02 KB
ddskerl_counters       0.0469 KB - 0.02x memory usage -2.97656 KB
ddskerl_ets            0.0469 KB - 0.02x memory usage -2.97656 KB
ddskerl_std              4.52 KB - 1.50x memory usage +1.50 KB
exact                 2565.74 KB - 848.62x memory usage +2562.72 KB

**All measurements for memory usage were the same**

Reduction count statistics:

Name             Reduction count
ddskerl_bound                261
ddskerl_counters             194 - 0.74x reduction count -67
ddskerl_ets                  103 - 0.39x reduction count -158
ddskerl_std                 1020 - 3.91x reduction count +759
exact                     459646 - 1761.10x reduction count +459385

**All measurements for reduction count were the same**
```

##### With input 1_000_000, error 1%, buckets 2000, quantile 0.95 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_counters      361.95 K        2.76 μs    ±51.53%        2.04 μs        6.21 μs
ddskerl_bound         361.33 K        2.77 μs    ±59.91%        2.19 μs        7.04 μs
ddskerl_std           113.98 K        8.77 μs    ±52.89%        7.02 μs       25.54 μs
ddskerl_ets           105.27 K        9.50 μs     ±5.05%        9.48 μs       10.25 μs
exact                0.00301 K   332500.89 μs     ±8.36%   321303.08 μs   384304.73 μs

Comparison:
ddskerl_counters      361.95 K
ddskerl_bound         361.33 K - 1.00x slower +0.00467 μs
ddskerl_std           113.98 K - 3.18x slower +6.01 μs
ddskerl_ets           105.27 K - 3.44x slower +6.74 μs
exact                0.00301 K - 120347.23x slower +332498.12 μs

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters         1.38 μs        6.21 μs             91                  1.63 μs
ddskerl_bound            1.08 μs        7.04 μs             50                     2 μs
ddskerl_std              3.21 μs       25.54 μs             70                     7 μs
ddskerl_ets              8.88 μs       10.25 μs              6                     None
exact               306272.43 μs   384304.73 μs              8                     None

Memory usage statistics:

Name                Memory usage
ddskerl_counters       0.0469 KB
ddskerl_bound            4.64 KB - 99.00x memory usage +4.59 KB
ddskerl_std              8.13 KB - 173.33x memory usage +8.08 KB
ddskerl_ets            0.0469 KB - 1.00x memory usage +0 KB
exact               340904.75 KB - 7272634.67x memory usage +340904.70 KB

**All measurements for memory usage were the same**

Reduction count statistics:

Name                     average  deviation         median         99th %
ddskerl_counters             286     ±0.00%            286            286
ddskerl_bound                430     ±0.00%            430            430
ddskerl_std                 1618     ±0.00%           1618           1618
ddskerl_ets                  276     ±0.00%            276            276
exact                65063267.50     ±0.04%       65056850       65097416

Comparison:
ddskerl_counters             286
ddskerl_bound                430 - 1.50x reduction count +144
ddskerl_std                 1618 - 5.66x reduction count +1332
ddskerl_ets                  276 - 0.97x reduction count -10
exact                65063267.50 - 227493.94x reduction count +65062981.50

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters             286            286             90                      286
ddskerl_bound                430            430             51                      430
ddskerl_std                 1618           1618             68                     1618
ddskerl_ets                  276            276              6                      276
exact                   65036000       65097416              8                     None
```


## Benchmark: `insert_in_parallel_one`
```
warmup: 2 s
time: 5 s
```

##### With input 100_000 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_counters          2.24         0.45 s     ±7.77%         0.44 s         0.50 s
ddskerl_ets               0.53         1.88 s     ±0.44%         1.88 s         1.88 s

Comparison:
ddskerl_counters          2.24
ddskerl_ets               0.53 - 4.21x slower +1.43 s
```

##### With input 100_000 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_counters          2.24         0.45 s     ±7.77%         0.44 s         0.50 s
ddskerl_ets               0.53         1.88 s     ±0.44%         1.88 s         1.88 s

Comparison:
ddskerl_counters          2.24
ddskerl_ets               0.53 - 4.21x slower +1.43 s

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters          0.40 s         0.50 s             12                     None
ddskerl_ets               1.87 s         1.88 s              3                     None
```

##### With input 10_000 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_counters         30.82       32.45 ms    ±14.74%       30.93 ms       58.41 ms
ddskerl_ets               5.50      181.95 ms     ±9.28%      187.16 ms      212.86 ms

Comparison:
ddskerl_counters         30.82
ddskerl_ets               5.50 - 5.61x slower +149.50 ms

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters        28.65 ms       58.93 ms            155                     None
ddskerl_ets            141.85 ms      212.86 ms             28                     None
```

##### With input 10_000_000 #####
```
Name                       ips        average  deviation         median         99th %
ddskerl_counters        0.0109       1.52 min     ±0.00%       1.52 min       1.52 min
ddskerl_ets            0.00425       3.93 min     ±0.00%       3.93 min       3.93 min

Comparison:
ddskerl_counters        0.0109
ddskerl_ets            0.00425 - 2.58x slower +2.40 min

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters        1.52 min       1.52 min              1                     None
```


## Benchmark: `insert_in_parallel`: insert on the same entry for lock-contention

Also note that while for both options CPU was maxed-out for the whole run of the benchmark,
for counters, CPU was entirely in user-space, while for ets it was 80% of the time in system calls.

Note in any case that this are measurements for a single insert, in a scenario when the entire VM is
highly saturated trying to run more of the same inserts, hence lock-contention is maxed-out.

```
warmup: 2 s
time: 30 s
parallel: 36
inputs: 100

Name                       ips        average  deviation         median         99th %
ddskerl_counters        1.63 M        0.61 μs  ±7365.97%       0.146 μs        6.03 μs
ddskerl_ets          0.00346 M      289.05 μs  ±1676.21%      181.83 μs      366.58 μs

Comparison:
ddskerl_counters        1.63 M
ddskerl_ets          0.00346 M - 470.88x slower +288.44 μs

Extended statistics:

Name                     minimum        maximum    sample size                     mode
ddskerl_counters            0 μs   118304.04 μs       276.04 M                0.0830 μs
ddskerl_ets              0.83 μs   396567.71 μs         1.93 M                  0.88 μs
```
