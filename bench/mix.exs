defmodule Bench.MixProject do
  use Mix.Project

  def project do
    [
      app: :bench,
      version: "0.1.0",
      elixir: "~> 1.18",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      erlc_paths: ["../src"]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    []
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:benchee, "~> 1.3"},
      {:benchee_csv, "~> 1.0"},
      {:benchee_html, "~> 1.0"},
      {:benchee_markdown, "~> 0.3.3"},
    ]
  end
end
