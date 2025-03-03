defmodule Bench do
  def insert_many_uniques() do
    :ets.new(:sequence, [:set, :public, :named_table])

    Benchee.run(
      %{
        "exact" => fn {range, _, _, _} ->
          Enum.reduce(range, :ddskerl_exact.new(), fn x, sd ->
            :ddskerl_exact.insert(sd, x)
          end)
        end,
        "ddskerl_std" => fn {range, error, _, _} ->
          Enum.reduce(range, :ddskerl_std.new(%{error: error}), fn x, sd ->
            :ddskerl_std.insert(sd, x)
          end)
        end,
        "ddskerl_bound" => fn {range, error, bound, _} ->
          Enum.reduce(range, :ddskerl_bound.new(%{error: error, bound: bound}), fn x, sd ->
            :ddskerl_bound.insert(sd, x)
          end)
        end,
        "ddskerl_counters" => fn {range, error, bound, _} ->
          Enum.reduce(range, :ddskerl_counters.new(%{error: error, bound: bound}), fn x, sd ->
            :ddskerl_counters.insert(sd, x)
          end)
        end,
        "ddskerl_ets" => fn {range, error, bound, name} ->
          opts = %{ets_table: :sequence, name: name, error: error, bound: bound}

          Enum.reduce(range, :ddskerl_ets.new(opts), fn x, sd ->
            :ddskerl_ets.insert(sd, x)
          end)
        end
      },
      inputs: %{
        "10_000, error 1%" => {
          Stream.repeatedly(fn -> Enum.random(0..100_000_000) end)
          |> Stream.uniq()
          |> Enum.take(10_000),
          0.01,
          2000,
          make_ref()
        },
        "10_000, error 2%" =>
          {Stream.repeatedly(fn -> Enum.random(0..100_000_000) end)
           |> Stream.uniq()
           |> Enum.take(10_000), 0.02, 2000, make_ref()},
        "100_000, error 1%" =>
          {Stream.repeatedly(fn -> Enum.random(0..100_000_000) end)
           |> Stream.uniq()
           |> Enum.take(100_000), 0.01, 2000, make_ref()},
        "1_000_000, error 1%" =>
          {Stream.repeatedly(fn -> Enum.random(0..100_000_000) end)
           |> Stream.uniq()
           |> Enum.take(1_000_000), 0.01, 2000, make_ref()}
      },
      reduction_time: 5,
      memory_time: 5,
      time: 5
    )
  end

  def insert_sequence() do
    :ets.new(:sequence, [:set, :public, :named_table])

    Benchee.run(
      %{
        "exact" => fn {range, _, _, _} ->
          Enum.reduce(range, :ddskerl_exact.new(), fn x, sd ->
            :ddskerl_exact.insert(sd, x)
          end)
        end,
        "ddskerl_std" => fn {range, error, _, _} ->
          Enum.reduce(range, :ddskerl_std.new(%{error: error}), fn x, sd ->
            :ddskerl_std.insert(sd, x)
          end)
        end,
        "ddskerl_bound" => fn {range, error, bound, _} ->
          Enum.reduce(range, :ddskerl_bound.new(%{error: error, bound: bound}), fn x, sd ->
            :ddskerl_bound.insert(sd, x)
          end)
        end,
        "ddskerl_counters" => fn {range, error, bound, _} ->
          Enum.reduce(range, :ddskerl_counters.new(%{error: error, bound: bound}), fn x, sd ->
            :ddskerl_counters.insert(sd, x)
          end)
        end,
        "ddskerl_ets" => fn {range, error, bound, name} ->
          opts = %{ets_table: :sequence, name: name, error: error, bound: bound}

          Enum.reduce(range, :ddskerl_ets.new(opts), fn x, sd ->
            :ddskerl_ets.insert(sd, x)
          end)
        end
      },
      inputs: %{
        "10_000, error 1%" => {1..10_000, 0.1, 2000, make_ref()},
        "100_000, error 1%" => {1..100_000, 0.1, 2000, make_ref()},
        "1_000_000, error 1%" => {1..1_000_000, 0.1, 2000, make_ref()}
      },
      reduction_time: 5,
      memory_time: 5,
      time: 5
    )
  end

  def query_quantiles_over_sequential_input() do
    :ets.new(:sequence, [:set, :public, :named_table])

    Benchee.run(
      %{
        "exact" =>
          {fn {ddsketch, quantile} ->
             :ddskerl_exact.quantile(ddsketch, quantile)
           end,
           before_each: fn {range, _, _, _, q} ->
             {Enum.reduce(range, :ddskerl_exact.new(), fn x, sd ->
                :ddskerl_exact.insert(sd, x)
              end), q}
           end},
        "ddskerl_std" =>
          {fn {ddsketch, quantile} ->
             :ddskerl_std.quantile(ddsketch, quantile)
           end,
           before_each: fn {range, error, _, _, q} ->
             {Enum.reduce(range, :ddskerl_std.new(%{error: error}), fn x, sd ->
                :ddskerl_std.insert(sd, x)
              end), q}
           end},
        "ddskerl_bound" =>
          {fn {ddsketch, quantile} ->
             :ddskerl_bound.quantile(ddsketch, quantile)
           end,
           before_each: fn {range, error, bound, _, q} ->
             {Enum.reduce(range, :ddskerl_bound.new(%{error: error, bound: bound}), fn x, sd ->
                :ddskerl_bound.insert(sd, x)
              end), q}
           end},
        "ddskerl_counters" =>
          {fn {ddsketch, quantile} ->
             :ddskerl_counters.quantile(ddsketch, quantile)
           end,
           before_each: fn {range, error, bound, _, q} ->
             {Enum.reduce(range, :ddskerl_counters.new(%{error: error, bound: bound}), fn x, sd ->
                :ddskerl_counters.insert(sd, x)
              end), q}
           end},
        "ddskerl_ets" =>
          {fn {ddsketch, quantile} ->
             :ddskerl_ets.quantile(ddsketch, quantile)
           end,
           before_each: fn {range, error, bound, name, q} ->
             opts = %{ets_table: :sequence, name: name, error: error, bound: bound}

             {Enum.reduce(range, :ddskerl_ets.new(opts), fn x, sd ->
                :ddskerl_ets.insert(sd, x)
              end), q}
           end}
      },
      inputs: %{
        "10_000, error 1%, quantile 0.5" => {1..10_000, 0.1, 2000, make_ref(), 0.5},
        "10_000, error 1%, quantile 0.9" => {1..10_000, 0.1, 2000, make_ref(), 0.9},
        "10_000, error 1%, quantile 0.95" => {1..10_000, 0.1, 2000, make_ref(), 0.95},
        "100_000, error 1%" => {1..100_000, 0.1, 2000, make_ref()},
        "1_000_000, error 1%" => {1..1_000_000, 0.1, 2000, make_ref()}
      },
      reduction_time: 5,
      memory_time: 5,
      time: 5
    )
  end
end
