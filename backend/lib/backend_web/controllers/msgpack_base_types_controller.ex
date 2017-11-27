defmodule BackendWeb.MsgpackBaseTypesController do
  use BackendWeb, :controller
  require Msgpax

  def index(conn, _params) do
    {:ok, packed} = Msgpax.pack(%{foo: "bar", names: %{john: 32, jane: 33}, nums: [-5, 12, -2038, 4096, 100200, -72150], pi: 3.14})
    Plug.Conn.put_resp_header(conn, "content-type", "text/plain; charset=x-user-defined")
        |> Plug.Conn.resp(200, IO.iodata_to_binary(packed))
  end
end
