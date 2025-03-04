defmodule Bench do
  @table :sequence
  @ets_opts [:set, :public, :named_table, read_concurrency: true, write_concurrency: true]

  def all() do
    insert_many_uniques()
    insert_sequence()
    insert_one_new_over_a_full_sketch()
    query_quantiles_over_sequential_input()
  end

  def insert_in_parallel() do
    :ets.new(@table, @ets_opts)
    :persistent_term.put(:ddskerl_counters, :ddskerl_counters.new(%{error: 0.01, bound: 2000}))
    name = make_ref()
    :ddskerl_ets.new(%{ets_table: @table, name: name, error: 0.01, bound: 2000})

    Benchee.run(
      %{
        "ddskerl_counters" =>
          {fn x ->
             :ddskerl_counters.insert(:persistent_term.get(:ddskerl_counters), x)
           end,
           before_each: fn _ ->
             Enum.random(1..1_000_000_000_000)
           end},
        "ddskerl_ets" =>
          {fn x ->
             :ddskerl_ets.insert(@table, name, x)
           end,
           before_each: fn _ ->
             Enum.random(1..1_000_000_000_000)
           end}
      },
      formatters: [{Benchee.Formatters.Console, extended_statistics: true}],
      print: [fast_warning: false],
      parallel: 36,
      time: 30
    )

    :persistent_term.erase(:ddskerl_counters)
    :ets.delete(@table)
  end

  def insert_in_parallel_one() do
    :ets.new(@table, @ets_opts)
    :persistent_term.put(:ddskerl_counters, :ddskerl_counters.new(%{error: 0.01, bound: 2000}))

    Benchee.run(
      %{
        "ddskerl_counters" =>
          {fn range ->
             tasks =
               Enum.map(range, fn x ->
                 Task.async(fn ->
                   :ddskerl_counters.insert(:persistent_term.get(:ddskerl_counters), x)
                 end)
               end)

             Task.await_many(tasks)
           end,
           before_scenario: fn range ->
             new = :ddskerl_counters.new(%{error: 0.01, bound: 2000})
             :persistent_term.put(:ddskerl_counters, new)
             range
           end,
           after_scenario: fn _ ->
             :persistent_term.erase(:ddskerl_counters)
           end},
        "ddskerl_ets" =>
          {fn {range, name} ->
             tasks =
               Enum.map(range, fn x ->
                 Task.async(fn ->
                   :ddskerl_ets.insert(@table, name, x)
                 end)
               end)

             Task.await_many(tasks)
           end,
           before_scenario: fn range ->
             name = make_ref()
             :ddskerl_ets.new(%{ets_table: @table, name: name, error: 0.01, bound: 2000})
             {range, name}
           end,
           after_scenario: fn _ ->
             :ets.delete_all_objects(@table)
           end}
      },
      inputs: %{
        "10_000" => sample_unique_values(10_000),
        "100_000" => sample_unique_values(100_000),
        "1_000_000" => sample_unique_values(1_000_000),
        "10_000_000" => sample_unique_values(10_000_000)
      },
      formatters: [{Benchee.Formatters.Console, extended_statistics: true}],
      print: [fast_warning: false],
      time: 5
    )

    :ets.delete(@table)
  end

  def insert_many_uniques() do
    :ets.new(@table, @ets_opts)

    Benchee.run(
      %{
        "exact" => fn {range, _, _} ->
          Enum.reduce(range, :ddskerl_exact.new(), fn x, sd ->
            :ddskerl_exact.insert(sd, x)
          end)
        end,
        "ddskerl_std" => fn {range, error, _} ->
          Enum.reduce(range, :ddskerl_std.new(%{error: error}), fn x, sd ->
            :ddskerl_std.insert(sd, x)
          end)
        end,
        "ddskerl_bound" => fn {range, error, bound} ->
          Enum.reduce(range, :ddskerl_bound.new(%{error: error, bound: bound}), fn x, sd ->
            :ddskerl_bound.insert(sd, x)
          end)
        end,
        "ddskerl_counters" => fn {range, error, bound} ->
          Enum.reduce(range, :ddskerl_counters.new(%{error: error, bound: bound}), fn x, sd ->
            :ddskerl_counters.insert(sd, x)
          end)
        end,
        "ddskerl_ets" => fn {range, error, bound} ->
          opts = %{ets_table: @table, name: make_ref(), error: error, bound: bound}

          Enum.reduce(range, :ddskerl_ets.new(opts), fn x, sd ->
            :ddskerl_ets.insert(sd, x)
          end)
        end
      },
      inputs: %{
        "10_000, error 2%, buckets 700" => {sample_unique_values(10_000), 0.02, 2000},
        "10_000, error 1%, buckets 2000" => {sample_unique_values(10_000), 0.01, 2000},
        "100_000, error 1%, buckets 2000" => {sample_unique_values(100_000), 0.01, 2000},
        "1_000_000, error 1%, buckets 2000" => {sample_unique_values(1_000_000), 0.01, 2000},
        "10_000_000, error 1%, buckets 2000" => {sample_unique_values(10_000_000), 0.01, 2000}
      },
      formatters: [{Benchee.Formatters.Console, extended_statistics: true}],
      print: [fast_warning: false],
      memory_time: 5,
      time: 5
    )

    :ets.delete(@table)
  end

  def insert_sequence() do
    :ets.new(@table, @ets_opts)

    Benchee.run(
      %{
        "exact" => fn {range, _, _} ->
          Enum.reduce(range, :ddskerl_exact.new(), fn x, sd ->
            :ddskerl_exact.insert(sd, x)
          end)
        end,
        "ddskerl_std" => fn {range, error, _} ->
          Enum.reduce(range, :ddskerl_std.new(%{error: error}), fn x, sd ->
            :ddskerl_std.insert(sd, x)
          end)
        end,
        "ddskerl_bound" => fn {range, error, bound} ->
          Enum.reduce(range, :ddskerl_bound.new(%{error: error, bound: bound}), fn x, sd ->
            :ddskerl_bound.insert(sd, x)
          end)
        end,
        "ddskerl_counters" => fn {range, error, bound} ->
          Enum.reduce(range, :ddskerl_counters.new(%{error: error, bound: bound}), fn x, sd ->
            :ddskerl_counters.insert(sd, x)
          end)
        end,
        "ddskerl_ets" => fn {range, error, bound} ->
          opts = %{ets_table: @table, name: make_ref(), error: error, bound: bound}

          Enum.reduce(range, :ddskerl_ets.new(opts), fn x, sd ->
            :ddskerl_ets.insert(sd, x)
          end)
        end
      },
      inputs: %{
        "10_000, error 1%, buckets 1000" => {1..10_000, 0.1, 1000},
        "10_000, error 1%, buckets 2000" => {1..10_000, 0.1, 2000},
        "100_000, error 1%, buckets 1000" => {1..100_000, 0.1, 1000},
        "100_000, error 1%, buckets 2000" => {1..100_000, 0.1, 2000},
        "1_000_000, error 1%, buckets 1000" => {1..1_000_000, 0.1, 1000},
        "1_000_000, error 1%, buckets 2000" => {1..1_000_000, 0.1, 2000}
      },
      formatters: [{Benchee.Formatters.Console, extended_statistics: true}],
      print: [fast_warning: false],
      memory_time: 5,
      time: 5
    )

    :ets.delete(@table)
  end

  def insert_one_new_over_a_full_sketch() do
    :ets.new(@table, @ets_opts)

    Benchee.run(
      %{
        "exact" =>
          {fn {ddsketch, insert} ->
             :ddskerl_exact.insert(ddsketch, insert)
           end,
           before_each: fn {[q | range], _, _} ->
             {Enum.reduce(range, :ddskerl_exact.new(), fn x, sd ->
                :ddskerl_exact.insert(sd, x)
              end), q}
           end},
        "ddskerl_std" =>
          {fn {ddsketch, insert} ->
             :ddskerl_std.insert(ddsketch, insert)
           end,
           before_each: fn {[q | range], error, _} ->
             {Enum.reduce(range, :ddskerl_std.new(%{error: error}), fn x, sd ->
                :ddskerl_std.insert(sd, x)
              end), q}
           end},
        "ddskerl_bound" =>
          {fn {ddsketch, insert} ->
             :ddskerl_bound.insert(ddsketch, insert)
           end,
           before_each: fn {[q | range], error, bound} ->
             {Enum.reduce(range, :ddskerl_bound.new(%{error: error, bound: bound}), fn x, sd ->
                :ddskerl_bound.insert(sd, x)
              end), q}
           end},
        "ddskerl_counters" =>
          {fn {ddsketch, insert} ->
             :ddskerl_counters.insert(ddsketch, insert)
           end,
           before_each: fn {[q | range], error, bound} ->
             {Enum.reduce(range, :ddskerl_counters.new(%{error: error, bound: bound}), fn x, sd ->
                :ddskerl_counters.insert(sd, x)
              end), q}
           end},
        "ddskerl_ets" =>
          {fn {ddsketch, insert} ->
             :ddskerl_ets.insert(ddsketch, insert)
           end,
           before_each: fn {[q | range], error, bound} ->
             opts = %{ets_table: @table, name: make_ref(), error: error, bound: bound}

             {Enum.reduce(range, :ddskerl_ets.new(opts), fn x, sd ->
                :ddskerl_ets.insert(sd, x)
              end), q}
           end}
      },
      inputs: %{
        "10_000, error 1%" => {sample_unique_values(10_001), 0.01, 2000},
        "100_000, error 1%" => {sample_unique_values(100_001), 0.01, 2000},
        "1_000_000, error 1%" => {sample_unique_values(1_000_001), 0.01, 2000},
        "10_000_000, error 1%, buckets 1000" => {sample_unique_values(10_000_001), 0.01, 1000},
        "10_000_000, error 1%, buckets 2000" => {sample_unique_values(10_000_001), 0.01, 2000}
      },
      formatters: [{Benchee.Formatters.Console, extended_statistics: true}],
      print: [fast_warning: false],
      memory_time: 15,
      time: 15
    )

    :ets.delete(@table)
  end

  def query_quantiles_over_sequential_input() do
    :ets.new(@table, @ets_opts)

    Benchee.run(
      %{
        "exact" =>
          {fn {ddsketch, quantile} ->
             :ddskerl_exact.quantile(ddsketch, quantile)
           end,
           before_each: fn {range, _, _, q} ->
             {Enum.reduce(range, :ddskerl_exact.new(), fn x, sd ->
                :ddskerl_exact.insert(sd, x)
              end), q}
           end},
        "ddskerl_std" =>
          {fn {ddsketch, quantile} ->
             :ddskerl_std.quantile(ddsketch, quantile)
           end,
           before_each: fn {range, error, _, q} ->
             {Enum.reduce(range, :ddskerl_std.new(%{error: error}), fn x, sd ->
                :ddskerl_std.insert(sd, x)
              end), q}
           end},
        "ddskerl_bound" =>
          {fn {ddsketch, quantile} ->
             :ddskerl_bound.quantile(ddsketch, quantile)
           end,
           before_each: fn {range, error, bound, q} ->
             {Enum.reduce(range, :ddskerl_bound.new(%{error: error, bound: bound}), fn x, sd ->
                :ddskerl_bound.insert(sd, x)
              end), q}
           end},
        "ddskerl_counters" =>
          {fn {ddsketch, quantile} ->
             :ddskerl_counters.quantile(ddsketch, quantile)
           end,
           before_each: fn {range, error, bound, q} ->
             {Enum.reduce(range, :ddskerl_counters.new(%{error: error, bound: bound}), fn x, sd ->
                :ddskerl_counters.insert(sd, x)
              end), q}
           end},
        "ddskerl_ets" =>
          {fn {ddsketch, quantile} ->
             :ddskerl_ets.quantile(ddsketch, quantile)
           end,
           before_each: fn {range, error, bound, q} ->
             opts = %{ets_table: @table, name: make_ref(), error: error, bound: bound}

             {Enum.reduce(range, :ddskerl_ets.new(opts), fn x, sd ->
                :ddskerl_ets.insert(sd, x)
              end), q}
           end}
      },
      inputs: %{
        "10_000, error 1%, buckets 500, quantile 0.95" => {1..10_000, 0.1, 500, 0.95},
        "10_000, error 1%, buckets 2000, quantile 0.95" => {1..10_000, 0.1, 2000, 0.95},
        "100_000, error 1%, buckets 500, quantile 0.5" => {1..100_000, 0.1, 500, 0.5},
        "100_000, error 1%, buckets 2000, quantile 0.5" => {1..100_000, 0.1, 2000, 0.5},
        "100_000, error 1%, buckets 500, quantile 0.9" => {1..100_000, 0.1, 500, 0.9},
        "100_000, error 1%, buckets 2000, quantile 0.9" => {1..100_000, 0.1, 2000, 0.9},
        "100_000, error 1%, buckets 500, quantile 0.95" => {1..100_000, 0.1, 500, 0.95},
        "100_000, error 1%, buckets 2000, quantile 0.95" => {1..100_000, 0.1, 2000, 0.95},
        "100_000, error 1%, buckets 500, quantile 0.999" => {1..100_000, 0.1, 500, 0.999},
        "100_000, error 1%, buckets 2000, quantile 0.999" => {1..100_000, 0.1, 2000, 0.999},
        "1_000_000, error 1%, buckets 2000, quantile 0.95" => {1..1_000_000, 0.1, 2000, 0.95}
      },
      formatters: [{Benchee.Formatters.Console, extended_statistics: true}],
      print: [fast_warning: false],
      memory_time: 5,
      time: 5
    )

    :ets.delete(@table)
  end

  defp sample_unique_values(num) do
    Stream.repeatedly(fn -> Enum.random(0..1_000_000_000_000) end)
    |> Stream.uniq()
    |> Enum.take(num)
  end
end
