# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

defmodule ErldistFilterElixirTests.MixProject do
  use Mix.Project

  def project() do
    [
      app: :erldist_filter_elixir_tests,
      version: "1.28.2",
      elixir: "~> 1.17",
      deps: deps(),
      elixirc_paths: elixirc_paths(Mix.env())
    ]
  end

  def application() do
    [extra_applications: [:logger, :observer, :runtime_tools, :erldist_filter, :erldist_filter_test]]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps() do
    [
      {:erldist_filter, path: "apps/erldist_filter", override: true},
      {:erldist_filter_test, path: "apps/erldist_filter_test", override: true}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]
end
