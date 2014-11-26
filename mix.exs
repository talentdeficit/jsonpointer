defmodule JSONPointer.Mixfile do
use Mix.Project

  def project do
    [
      app: :jsonpointer,
      version: "1.1.0",
      description: "a tiny library to convert to and from json pointer syntax",
      package: package,
      language: :erlang,
      # enable support for maps in jsx
      erlc_options: [] ++ extra_opts(Mix.env)
    ]
  end

  defp extra_opts(:dev), do: [d: :TEST]
  defp extra_opts(_), do: []

  defp package do
    [
      files: [
        "LICENSE",
        "mix.exs",
        "README.md",
        "src"
      ],
      contributors: ["alisdair sullivan"],
      links: %{"github" => "https://github.com/talentdeficit/jsonpointer"},
      licenses: ["MIT"]
    ]
  end
end