package multiplayer

import utest._
import minesweeper._
//import GameStateProtocol._
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit
import cask.endpoints.WsChannelActor
import io.undertow.websockets.core.WebSocketChannel
import castor.Context.Simple.global
import cask.Logger.Console.globalLogger
import upickle.default._

import routes._
import cask.main.Routes
import routes.MultiPlayerProtocol.Game

object MultiPlayerRoutesSuite extends TestSuite {

  override val tests = Tests {
    test("list games") - withServer(new TestServer()) { (host, _) =>
      val r = requests.get(host + s"/games")

      val json = ujson.read(r.text())

      json.arr.length ==> 2
    }
    test("create") - withServer(new TestServer()) { (host, _) =>
      val name = "game of games"
      val r = requests.post(host + s"/games", data = s"""{"name": "$name"}""")

      val json = ujson.read(r.text())

      json("id").str.length ==> 36
    }

    test("socket") - withServer(new TestServer()) { (_, ws) =>
      import GameStateProtocol._
      import MultiPlayerSocketProtocol._
      @volatile var user1 = User("", -1)
      @volatile var user2 = User("", -1)
      @volatile var words1 = Seq[Word]()
      @volatile var words2 = Seq[Word]()

      val wsClient1 = send(
        cask.WsClient.connect(ws + "/gamesconnect/gid1") {
          case cask.Ws.Text(s) =>
            read[WsResponse](s) match {
              case u1: User if u1.uid == "ove" => user1 = u1
              case wds: Words                  => words1 = wds.words
              case x                           => println(s"ignored: $x")
            }
        },
        _
      )
      Thread.sleep(200)
      val wsClient2 = send(
        cask.WsClient.connect(ws + "/gamesconnect/gid1") {
          case cask.Ws.Text(s) =>
            read[WsResponse](s) match {
              case u: User if u.uid == "dallas" => user2 = u
              case wds: Words                   => words2 = wds.words
              case x                            => println(s"ignored: $x")
            }
        },
        _
      )
      wsClient1(WsJoin("ove"))
      Thread.sleep(200)
      wsClient2(WsJoin("dallas"))
      Thread.sleep(200)
      user1 ==> User("ove", 0)
      user2 ==> User("dallas", 0)
      assert(words1.isEmpty)
      assert(words2.isEmpty)

      wsClient1(WsStart("gid1"))
      Thread.sleep(200)
      assert(words1.nonEmpty)
      assert(words2.nonEmpty)

      words1.foreach { wo =>
        val w = wo.word
        if scala.util.Random.nextBoolean then
          wsClient1(WsText(w))
          wsClient2(WsText(w))
        else
          wsClient2(WsText(w))
          wsClient1(WsText(w))

        Thread.sleep(10)
      }
      Thread.sleep(100)
      val allWords = user1.words + user2.words
      allWords ==> 5
    }

    test("create, join, play game") - withServer(new TestServer()) { (host, ws) =>
      import GameStateProtocol._
      import MultiPlayerSocketProtocol._
      @volatile var user1 = User("", -1)
      @volatile var words1 = Seq[Word]()

      val r = requests.post(host + s"/games", data = s"""{"name": "testgame 42"}""")
      val gid = ujson.read(r.text())("id").str

      val wsClient1 = send(
        cask.WsClient.connect(ws + s"$ws/gamesconnect/$gid") {
          case cask.Ws.Text(s) =>
            read[WsResponse](s) match {
              case u1: User if u1.uid == "ove" => user1 = u1
              case wds: Words                  => words1 = wds.words
              case x                           => println(s"ignored: $x")
            }
        },
        _
      )
      Thread.sleep(200)
    }
  }

  class TestServer() extends cask.Main {
    val allRoutes = Seq(
      MultiPlayerRoutes(
        new MultiplayerBackendInMem(
          Map(
            "gid1" -> Game("gid1", "game1"),
            "gid2" -> Game("gid2", "game2")
          )
        )
      )
    )
  }
}
