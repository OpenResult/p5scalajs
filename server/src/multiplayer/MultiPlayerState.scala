package multiplayer

import cask.endpoints.WsChannelActor
import scala.util.Random._
import java.io.File
import upickle.default._
import java.util.UUID
import multiplayer.MultiPlayerSocketProtocol._

object GameStateProtocol:
  sealed trait GameMsg
  case class Join(uid: String, channel: WsChannelActor) extends GameMsg
  case class Start(gid: String) extends GameMsg
  case class Text(uid: String, s: String) extends GameMsg
  case class Leave(uid: String) extends GameMsg

class GameState(gid: String)(implicit
    ac: castor.Context
) extends castor.StateMachineActor[GameStateProtocol.GameMsg] {
  import GameStateProtocol._
  // override def run(msg: GameMsg): Unit = {
  //   print(s"$state + $msg -> ")
  //   super.run(msg)
  //   println(state)
  // }
  lazy val id = UUID.randomUUID.toString.subSequence(0, 5)
  def initialState = Created()
  var uidToChannel: Map[String, WsChannelActor] = Map()

  case class Created(players: Map[String, User] = Map.empty)
      extends State({
        case Join(uid, channel) =>
          val user = User(uid, 0)
          uidToChannel = uidToChannel + (uid -> channel)
          broadcast(user)
          Created(players + (uid -> user))
        case Start(id) if id == gid =>
          val ws = words(5)
          broadcast(ws)
          println(ws)
          Started(players, ws.words)
        case x =>
          println(s"Unhandled: $x")
          state
      })
  case class Started(players: Map[String, User], words: Seq[Word])
      extends State({
        case Text(uid, word) if words.find(_.word == word).isDefined && players.contains(uid) =>
          val user = players(uid)
          val u = user.copy(words = user.words + 1)
          val newWords = words.filterNot(_.word == word)

          broadcast(Words(newWords))
          broadcast(u)
          Started(
            players.updated(uid, u),
            newWords
          )
        case Leave(uid) =>
          uidToChannel = uidToChannel.removed(uid)
          Started(players.removed(uid), words)
        case x =>
          println(s"Unhandled: $x")
          state
      })
  private def broadcast(msg: WsResponse): Unit =
    import MultiPlayerSocketProtocol.webSocketResponseRW
    val e = cask.Ws.Text(write(msg))
    uidToChannel.valuesIterator.foreach(_.send(e))

    println(s"broadcast($id) " + uidToChannel.keys)

  def words(count: Int): Words = {
    Words(
      wordsJson
        .take(count)
        .map { w => Word(w) }
    )
  }
  lazy val wordsJson: Seq[String] = {
    val f = File("./server/resources/svenska-ord.json")
    shuffle(read[Seq[String]](f))
  }
}
