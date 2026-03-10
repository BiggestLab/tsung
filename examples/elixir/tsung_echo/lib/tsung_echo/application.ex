defmodule TsungEcho.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      TsungEchoWeb.Telemetry,
      {DNSCluster, query: Application.get_env(:tsung_echo, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: TsungEcho.PubSub},
      # Start a worker by calling: TsungEcho.Worker.start_link(arg)
      # {TsungEcho.Worker, arg},
      # Start to serve requests, typically the last entry
      TsungEchoWeb.Endpoint
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: TsungEcho.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    TsungEchoWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
