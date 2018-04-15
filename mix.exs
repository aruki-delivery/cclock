defmodule CClock.Mixfile do
  use Mix.Project

  def project do
    [app: :cclock,
      version: "2.0.0",
      language: :erlang,
      deps: deps(Mix.env()),
      description: "Cluster clock",
      package: package(),
      source_url: "https://github.com/aruki-delivery/cclock",
      homepage_url: "https://hex.pm/packages/cclock",
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [coveralls: :test],
    ]
  end

  defp deps(_) do
    [
      {:dialyxir, "~> 0.5", only: [:dev], runtime: false},
      {:mix_test_watch, "~> 0.3", only: :dev, runtime: false},
      {:excoveralls, "~> 0.8", only: :test},
      {:columbo, "~> 2.0"},
      {:ex_doc, ">= 0.0.0", only: :dev}
    ]
  end

  def package do
    [ maintainers: ["cblage"],
      licenses: ["Apache License 2.0"],
      links: %{"GitHub" => "https://github.com/aruki-delivery/cclock" } ]
  end
end