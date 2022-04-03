import scala.scalajs.js
import js.annotation.JSExportTopLevel
import js.annotation.JSExport
import org.scalajs.dom.raw._

import org.scalajs.dom

import upickle.default._
import shared.Protocol
import P5VectorExt._

@JSExportTopLevel("Bounce")
class Bounce() extends js.Object {
  val sketch: js.Function1[Sketch, Unit] = { s =>
    import s._

    var ball: Ball = null
    var ball2: Ball = null

    setup = { () =>
      createCanvas(800, 400)
      ball = Ball(P5Vector(100, 100), P5Vector(1, 3.3), 75)
      ball2 = Ball(P5Vector(300, 300), P5Vector(1.5, 4.3), 55)
    }

    draw = { () =>
      background(232)
      ball = ball.move(s)
      ball.draw(s)
      ball2 = ball2.move(s)
      ball2.draw(s)
      if ball.intersect(ball2) then
        ball = ball.bounce(ball2)
        ball2 = ball2.bounce(ball)
      end if
    }
  }
}
case class Ball(pos: P5Vector, speed: P5Vector, diameter: Double) {
  lazy val radius = diameter / 2
  def draw(s: Sketch) =
    import s._
    fill(128, 245, 128)
    ellipse(pos.x, pos.y, diameter, diameter)

  def move(s: Sketch): Ball =
    import s._
    if (pos.x + radius > width) || (pos.x < radius) then {
      val newSpeed = speed.negX()
      copy(pos = pos + newSpeed, speed = newSpeed)
    }
    else if (pos.y + radius > height) || (pos.y < radius) then {
      val newSpeed = speed.negY()
      copy(pos = pos + newSpeed, speed = newSpeed)
    }
    else copy(pos = pos + speed)

  def intersect(other: Ball): Boolean =
    radius + other.radius > pos.dist(other.pos)

  // silly bounce, only reverse direction no matter angle of impact
  def bounce(other: Ball): Ball =
    copy(speed = speed.neg())

}
