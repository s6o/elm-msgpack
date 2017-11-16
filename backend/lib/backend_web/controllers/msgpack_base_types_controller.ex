defmodule BackendWeb.MsgpackBaseTypesController do
  use BackendWeb, :controller
  require Msgpax

  def index(conn, _params) do
    {:ok, packed} = Msgpax.pack(%{name: "John Doe"})
    Plug.Conn.put_resp_header(conn, "content-type", "text/plain; charset=iso-8859-1")
        |> Plug.Conn.resp(200, IO.iodata_to_binary(packed))
  end
end
