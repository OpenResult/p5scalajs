package typing

import scala.scalajs.js
import js.annotation.JSExportTopLevel
import p5._
import P5VectorExt._
import scala.util.Random.between
import org.scalajs.dom
import scala.scalajs.js.JSON
import upickle.default._
import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global
import multiplayer.MultiPlayerSocketProtocol.{WsResponse, User, Word, Words}
import multiplayer.MultiPlayerSocketProtocol.{WsText, WsJoin, WsStart}
import scala.util.Success

@JSExportTopLevel("Typing")
class Typing(textCallback: js.Function1[String, Unit]) extends js.Object {
  val sketch: js.Function1[Sketch, Unit] = { s =>
    import s._
    var texts = Seq[Text]()
    var currentText = ""
    var client: Option[WsClient] = None
    setup = () =>
      createCanvas(500, 500)
      client = {

        val c = new WsClient(
          "gid1",
          {
            case u: User => println(u)
            case w: Word => println(w)
            case ws: Words =>
              println("words")
              if texts.isEmpty then
                texts = ws.words
                  .map(w => Text.random(w.word))
              else
                texts = texts
                  .filter(t => ws.words.find(w => w.word == t.text).isDefined)
          }
        )
        c.onOpen.foreach { _ =>
          c.send(WsJoin("uid1"))
          c.send(WsStart("gid1"))
        }
        Some(c)
      }

    draw = () =>
      background(200)
      texts = texts
        .map { t =>
          t.move
            .draw(s)
        }
        .filterNot(t => t.pos.y > 600)

    keyPressed = event =>
      if event.key == "Backspace" then
        currentText = currentText.substring(0, currentText.size - 1)
      else if event.key == "Enter" then currentText = ""
      else
        currentText = currentText + event.key
        client.foreach { c =>
          c.send(WsText(currentText))
        }

      textCallback(currentText)
      true

  }
}
case class Text(text: String, pos: P5Vector, vel: P5Vector):
  def move: Text =
    copy(pos = pos + vel)

  def draw(s: Sketch): Text =
    s.text(text, pos.x, pos.y)
    this

object Text:
  def random(s: String): Text =
    Text(
      s,
      P5Vector(between(0, 400), between(-150, 0)),
      P5Vector(0, between(0.05, 0.2))
    )
