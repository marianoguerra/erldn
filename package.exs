defmodule Erldn.Mixfile do
  use Mix.Project

  @version File.read!("VERSION") |> String.strip

  def project do
    [app: :erldn,
    version: @version,
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
    [files: ~w(src README.rest LICENSE VERSION Makefile rebar),
    maintainers: ["Mariano Guerra"],
    licenses: ["MIT"],
    links: %{"GitHub" => "https://github.com/marianoguerra/erldn"}]
  end
end
