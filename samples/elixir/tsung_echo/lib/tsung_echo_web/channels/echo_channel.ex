defmodule TsungEchoWeb.EchoChannel do
  use Phoenix.Channel

  @impl true
  def join("echo:lobby", _payload, socket) do
    {:ok, %{status: "connected"}, socket}
  end

  def join("echo:" <> _room, _payload, socket) do
    {:ok, %{status: "connected"}, socket}
  end

  @impl true
  def handle_in("ping", payload, socket) do
    {:reply, {:ok, Map.put(payload, "response", "pong")}, socket}
  end

  def handle_in("echo", %{"message" => msg} = _payload, socket) do
    {:reply, {:ok, %{"echoed" => msg}}, socket}
  end

  def handle_in("broadcast", %{"message" => msg}, socket) do
    broadcast!(socket, "new_msg", %{message: msg})
    {:noreply, socket}
  end

  def handle_in(_event, _payload, socket) do
    {:reply, {:error, %{reason: "unknown event"}}, socket}
  end
end
