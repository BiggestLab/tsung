defmodule TsungEchoWeb.Router do
  use TsungEchoWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", TsungEchoWeb do
    pipe_through :api
    get "/health", HealthController, :index
  end
end
