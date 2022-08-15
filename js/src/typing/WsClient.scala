package typing

import org.scalajs.dom
import multiplayer.MultiPlayerSocketProtocol
import MultiPlayerSocketProtocol.WsRequest
import MultiPlayerSocketProtocol.WsResponse
import upickle.default._
import scala.concurrent.Future
import scala.concurrent.Promise

class WsClient(gid: String, onMessage: scala.scalajs.js.Function1[WsResponse, Unit]) {
  val socket = new dom.WebSocket(s"ws://localhost:8384/gamesconnect/$gid")
  private val onOpenPromise: Promise[Unit] = Promise()
  val onOpen: Future[Unit] = onOpenPromise.future
  socket.onmessage = { (e: dom.MessageEvent) =>
    import MultiPlayerSocketProtocol.webSocketResponseRW
    val msg = e.data.asInstanceOf[String]
    onMessage(read(msg))
  }
  socket.onopen = { (e: dom.Event) =>
    println("onconnect")
    onOpenPromise.success(())
  }

  def send(msg: WsRequest): WsClient =
    import MultiPlayerSocketProtocol.webSocketRequestRW
    socket.send(write(msg))
    this
}
