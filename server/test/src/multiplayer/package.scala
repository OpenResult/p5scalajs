import io.undertow.Undertow
package object multiplayer {
  def withServer[T](m: cask.main.Main)(f: (String, String) => T): T = {
    val port = 8289
    val server = Undertow.builder
      .addHttpListener(port, "localhost")
      .setHandler(m.defaultHandler)
      .build
    server.start()
    val res =
      try f(s"http://localhost:$port", s"ws://localhost:$port")
      finally server.stop()
    res
  }
  def send(c: cask.WsClient, msg: MultiPlayerSocketProtocol.WsRequest) =
    import MultiPlayerSocketProtocol.webSocketRequestRW
    import upickle.default._
    c.send(
      cask.Ws.Text(
        write(msg)
      )
    )
}
