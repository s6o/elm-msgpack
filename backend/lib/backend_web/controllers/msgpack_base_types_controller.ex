defmodule BackendWeb.MsgpackBaseTypesController do
  use BackendWeb, :controller
  require Msgpax

  def index(conn, _params) do
    {:ok, packed} = Msgpax.pack([2048, 80910, -39, -4096, -72150]) # 3.14159265358, -2.71828182845, "beer", %{foo: "bar"}])
    Plug.Conn.put_resp_header(conn, "content-type", "text/plain; charset=x-user-defined")
        |> Plug.Conn.resp(200, IO.iodata_to_binary(packed))
  end
end
