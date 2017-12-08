defmodule BackendWeb.MsgpackBaseTypesController do
  use BackendWeb, :controller
  require Msgpax
  require Msgpax.Bin
  require Msgpax.Ext

  def index(conn, _params) do
    {:ok, packed} = Msgpax.pack(%{
        binbytes: Msgpax.Bin.new(<<0xFF, 0xFE, 0xFD, 0xFC, 0xFB, 0xFA>>),
        bool_f: false,
        bool_t: true,
        days: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31],
        extbytes: Msgpax.Ext.new(0x07, <<0xAB, 0xAC, 0xAD, 0xAE, 0xAF>>),
        fixints: [1, 2, 3, 4, 5, 6, 7, 8, 9, 0, -1, -2, -3, -4, -5, -6, -7, -8, -9],
        fixmap: %{p1: "Jane Doe", p2: "John Doe"},
        floats: [1.25, 3.14, -0.25, 0.75],
        ints: [-2038, -100200, 4096, 72150],
        msglong: "Once upon a time in a galaxy far far away, yes, far far away ...",
        msgshort: "msgpack",
        msgutf: "öösel, õun, äädikas - at night, apple, vinegar",
        novalue: nil,
        x: "X",
        y: "Y",
        z: "Z",
    })
    Plug.Conn.put_resp_header(conn, "content-type", "text/plain; charset=x-user-defined")
        |> Plug.Conn.resp(200, IO.iodata_to_binary(packed))
  end
end
