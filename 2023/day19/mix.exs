defmodule Day19.MixProject do
  use Mix.Project

  def project do
    [
      app: :day19,
      version: "0.1.0",
      elixir: "~> 1.15",
      escript: [ main_module: Day19 ],
      deps: deps()
    ]
  end

  defp deps do
    [
      {:nimble_parsec, "~> 1.4"	}
    ]
  end
end
