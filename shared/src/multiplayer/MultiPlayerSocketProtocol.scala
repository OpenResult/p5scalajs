package multiplayer

  import upickle.default._

object MultiPlayerSocketProtocol:
  sealed trait WsRequest
  case class WsJoin(uid: String) extends WsRequest
  case class WsText(s: String) extends WsRequest
  case class WsStart(gid: String) extends WsRequest

  implicit val webSocketRequestRW: ReadWriter[WsRequest] = {
    implicit val jRW: ReadWriter[WsJoin] = macroRW[WsJoin]
    implicit val tRW: ReadWriter[WsText] = macroRW[WsText]
    implicit val sRW: ReadWriter[WsStart] = macroRW[WsStart]

    macroRW[WsRequest]
  }

  sealed trait WsResponse
  case class Word(word: String) extends WsResponse
  case class Words(words: Seq[Word]) extends WsResponse
  case class User(uid: String, words: Int) extends WsResponse
  case class Game(gid: String, name: String) extends WsResponse
  case class Games(gs: Seq[Game]) extends WsResponse

  implicit val webSocketResponseRW: ReadWriter[WsResponse] = {
    implicit val wRW: ReadWriter[Word] = macroRW[Word]
    implicit val wsRW: ReadWriter[Words] = macroRW[Words]
    implicit val uRW: ReadWriter[User] = macroRW[User]
    implicit val gRW: ReadWriter[Game] = macroRW[Game]
    implicit val gsRW: ReadWriter[Games] = macroRW[Games]

    macroRW[WsResponse]
  }
