package routes

import castor.SimpleActor
import cask.endpoints.WsChannelActor
import shared.Protocol

import upickle.default._
import multiplayer.GameState
import java.util.UUID

import io.undertow.Undertow
import multiplayer.MultiPlayerSocketProtocol

case class MultiPlayerRoutes(multiPlayerBackend: MultiplayerBackend)(implicit cc: castor.Context, log: cask.Logger)
    extends cask.Routes {

  @cask.postJson("/games")
  def createGame(name: String) = {
    val id = multiPlayerBackend.create(name)
    ujson.Obj {
      "id" -> id
    }
  }

  @cask.getJson("/games")
  def listPublicGames() = {
    multiPlayerBackend.list()
  }

  var games = Map[String, GameState]()

  @cask.websocket("/gamesconnect/:gid")
  def connect(gid: String): cask.WebsocketResult = {
    import multiplayer.GameStateProtocol._
    import multiplayer.MultiPlayerSocketProtocol._
    val game = multiPlayerBackend.get(gid)
    
    if (game.isEmpty) cask.Response("", statusCode = 403)
    else
      cask.WsHandler { channel =>
        val gameState = {
          val g = games.get(gid)
          if g.isEmpty then
            val _g = new GameState(gid)
            games = games + (gid -> _g)
            _g
          else
            g.get
        }

        val gs = multiPlayerBackend.list()

        val g = Games(gs.map(_.toWsGame))
        
        //val s = write(g)

        channel.send(cask.Ws.Text("dallas"))

        var uid = ""

        cask.WsActor {
          case cask.Ws.Text("") =>
            channel.send(cask.Ws.Close())
          case cask.Ws.Text(data) =>
            //println(data)
            read[WsRequest](data) match {
              case WsJoin(_uid) =>
                uid = _uid
                println(s"join $uid")
                gameState.send(Join(uid, channel))
              case WsText(t) =>
                gameState.send(Text(uid, t))
              case WsStart(gid) =>
                println(s"start $gid")
                gameState.send(Start(gid))
            }
          case cask.Ws.Close(_, _) =>
            gameState.send(Leave(uid))
            //Chat.send(ChatProtocol.MemberLeving(userName))
        }
      }
  }
  initialize()
}

object MultiPlayerProtocol {
  sealed trait Message
  case class Game(gid: String, name: String) extends Message:
    def toWsGame = MultiPlayerSocketProtocol.Game(gid, name)

  import upickle.default._
  implicit val messageRW: ReadWriter[Message] = {
    implicit val cmRW: ReadWriter[Game] = macroRW[Game]

    macroRW[Message]
  }
}
import MultiPlayerProtocol._
trait MultiplayerBackend:
  def get(id: String): Option[Game]
  def list(): Seq[Game]
  def create(name: String): String

class MultiplayerBackendInMem(var games: Map[String, Game] = Map()) extends MultiplayerBackend:
  override def get(id: String) = games.get(id)
  override def list() = games.values.toList
  override def create(name: String) =
    val id = UUID.randomUUID.toString
    val game = Game(id, name)
    games = games + (id -> game)
    id