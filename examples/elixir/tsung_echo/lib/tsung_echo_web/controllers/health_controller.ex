defmodule TsungEchoWeb.HealthController do
  use TsungEchoWeb, :controller

  def index(conn, _params) do
    json(conn, %{status: "ok"})
  end
end
