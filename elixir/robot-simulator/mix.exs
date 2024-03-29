defmodule RobotSimulator.MixProject do
  use Mix.Project

  def project do
    [
      app: :robot_simulator,
      version: "1.0.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      docs: [
        main: "RobotSimulator",
        extras: ["README.md"]
      ]
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:ex_doc, "~> 0.27", only: :dev, runtime: false}
    ]
  end
end
