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

@JSExportTopLevel("Typing")
class Typing(textCallback: js.Function1[String, Unit]) extends js.Object {
  val sketch: js.Function1[Sketch, Unit] = { s =>
    import s._
    var texts = Seq[Text]()
    var currentText = ""
    setup = () =>
      createCanvas(500, 500)
      fetchTexts()

    draw = () =>
      background(200)
      texts = texts
        .map { t =>
          t.move
            .draw(s)
        }
        .filterNot(t => t.pos.y > 600)
      
      if texts.size == 0 then
        fetchTexts()

    keyPressed = event =>
      if event.key == "Backspace" then
        currentText = currentText.substring(0, currentText.size - 1)
      else if event.key == "Enter" then currentText = ""
      else
        currentText = currentText + event.key
        texts
          .find(_.text == currentText)
          .foreach { t =>
            texts = texts.filterNot(_.text == currentText)
            currentText = ""
          }
      textCallback(currentText)
      true

    def fetchTexts() =
      FetchWords.get(15).foreach { xs =>
        texts = xs
          .map(
            Text(
              _,
              P5Vector(between(0, 400), between(-150, 0)),
              P5Vector(0, between(0.05, 0.2))
            )
          )
      }
  }
}
case class Text(text: String, pos: P5Vector, vel: P5Vector):
  def move: Text =
    copy(pos = pos + vel)

  def draw(s: Sketch): Text =
    s.text(text, pos.x, pos.y)
    this

object FetchWords {
  import scalajs.js.Thenable.Implicits.thenable2future
  def get(n: Int): Future[Seq[String]] =
    val responseText = for {
      response <- dom.fetch(s"/words?count=$n")
      text <- response.text()
    } yield {
      text
    }
    responseText.map(read[Seq[String]](_))
}