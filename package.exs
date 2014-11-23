defmodule Erldn.Mixfile do
  use Mix.Project

  def project do
    [app: :erldn,
    version: "1.0.0",
    description: description,
    package: package,
    deps: deps]
  end

  defp deps do
    []
  end

  defp description do
    """
    An edn parser for the Erlang platform.
    """
  end

  defp package do
    [files: ~w(src README.md LICENSE Makefile rebar),
    contributors: ["Mariano Guerra"],
    licenses: ["MIT"],
    links: %{"GitHub" => "https://github.com/marianoguerra/erldn"}]
  end
end
