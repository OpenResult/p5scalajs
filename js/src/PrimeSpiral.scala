import scala.scalajs.js
import js.annotation.JSExportTopLevel
import js.annotation.JSExport
import org.scalajs.dom.raw._

import org.scalajs.dom

import upickle.default._
import shared.Protocol
import p5._
import P5VectorExt._


@JSExportTopLevel("PrimeSpiral")
class PrimeSpiral() extends js.Object {
  val sketch: js.Function1[Sketch, Unit] = { s =>
    import s._
    setup = { () =>
      createCanvas(500, 500)
      background(255)
      Spiral(
        width,
        100,
        { (p1, p2) =>
            line(p1.x, p1.y, p2.x, p2.y)
            if p1.n != 1 && isPrime(p1.n) then
              fill("blue")
              p1.direction match {
                case Direction.East =>
                  triangle(p1.x, p1.y, p1.x - 5, p1.y - 2, p1.x - 5, p1.y + 2)
                case Direction.West =>
                  triangle(p1.x, p1.y, p1.x + 5, p1.y - 2, p1.x + 5, p1.y + 2)
                case Direction.North =>
                  triangle(p1.x, p1.y, p1.x - 2, p1.y + 5, p1.x + 2, p1.y + 5)
                case Direction.South =>
                  triangle(p1.x, p1.y, p1.x - 2, p1.y - 5, p1.x + 2, p1.y - 5)
              }
        }
      )
    }
    def isPrime(n: Int) = !(2 to n-1).exists(n % _ == 0)
  }
  case class Spiral(width: Double, columns: Int, f: (Position, Position) => Unit) {
    var steps = columns * columns
    var stepSize = width / (columns + 1)
    var maxStepsInSameDirection = 1
    var stepsInSameDirection = 0
    var first = true
    var previousPosition = Position(1, width / 2, width / 2)
    (1 to steps - 1).foreach { n =>
      val newPosition = if stepsInSameDirection == maxStepsInSameDirection then
        stepsInSameDirection = 1
        if first then
          first = false
        else
          maxStepsInSameDirection = maxStepsInSameDirection + 1
          first = true
        previousPosition.turnLeft().stepForward(stepSize)
      else
        stepsInSameDirection = stepsInSameDirection + 1
        previousPosition.stepForward(stepSize)

      f(previousPosition, newPosition)
      previousPosition = newPosition
    }
  }
  sealed trait Direction {
    def turnLeft: Direction
    def stepForward(steps: Double, p: Position): Position
  }
  object Direction {
    case object East extends Direction:
      def turnLeft = North
      def stepForward(s: Double, p: Position) = p.copy(x = p.x + s)
    case object West extends Direction:
      def turnLeft = South
      def stepForward(s: Double, p: Position) = p.copy(x = p.x - s)
    case object North extends Direction:
      def turnLeft = West
      def stepForward(s: Double, p: Position) = p.copy(y = p.y - s)
    case object South extends Direction:
      def turnLeft = East
      def stepForward(s: Double, p: Position) = p.copy(y = p.y + s)

  }
  case class Position(
      n: Int,
      x: Double,
      y: Double,
      direction: Direction = Direction.East
  ) {
    def stepForward(steps: Double): Position =
      direction.stepForward(steps, this.copy(n = n + 1))
    def turnLeft() = copy(direction = direction.turnLeft)
  }
}
