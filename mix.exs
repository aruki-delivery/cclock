defmodule CClock.Mixfile do
  use Mix.Project

  def project do
    [app: :cclock,
      version: "0.1.0",
      language: :erlang,
      deps: deps(Mix.env()),
      description: "Cluster clock",
      package: package(),
      source_url: "https://github.com/aruki-delivery/cclock",
      homepage_url: "https://hex.pm/packages/cclock"]
  end

  defp deps(_) do
    [{:columbo, "~> 0.1.0"},
      {:ex_doc, ">= 0.0.0", only: :dev}]
  end

  def package do
    [ maintainers: ["cblage"],
      licenses: ["Apache License 2.0"],
      links: %{"GitHub" => "https://github.com/aruki-delivery/cclock" } ]
  end
end