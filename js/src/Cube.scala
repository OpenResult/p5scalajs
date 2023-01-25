import scala.scalajs.js
import js.annotation.JSExportTopLevel
import matrix._
import matrix.MatrixVector
import matrix.MatrixVectorExt._
import matrix.Matrix
import matrix.Matrix._
import matrix.MatrixExt._
import p5._

@JSExportTopLevel("SpinningCube")
class SpinningCube() extends js.Object {
  val sketch: js.Function1[Sketch, Unit] = { s =>
    import s._

    var cube = Cube()

    setup = () =>
      createCanvas(500, 500)

    draw = () =>
      background(255)
      translate(width / 2, height / 2)
      val angle = millis() / 10000
      val rotation = rotationX(angle)
        .matmult(rotationY(angle))
        .matmult(rotationZ(angle))
      val c = cube
        .map(_.matmult(rotation))
        .map(_.projected)
        .map(_.mult(200))

      c.forConnections { (v1, v2) =>
        line(v1.x, v1.y, v2.x, v2.y)
      }
      c.forPoints { v =>
        val d = map(v.z, -200, 200, 4, 7)
        fill(0, 200, 0)
        circle(v.x, v.y, d)
      }
  }
}

object Cube:
  def apply(r: Double = 0.5) =
    new Cube(
      Seq(
        Seq(r, r, r),
        Seq(r, r, -r),
        Seq(-r, r, -r),
        Seq(-r, r, r),
        Seq(r, -r, r),
        Seq(r, -r, -r),
        Seq(-r, -r, -r),
        Seq(-r, -r, r)
      )
    )

class Cube(points: Matrix):
  def forPoints(f: MatrixVector => Unit) = points.foreach(f)
  def forConnections(f: (MatrixVector, MatrixVector) => Unit) =
    for (i <- 0 to 3) {
      f(points(i), points((i + 1) % 4))
      f(points(i + 4), points((i + 1) % 4 + 4))
      f(points(i), points(i + 4))
    }

  def map(f: Matrix => Matrix): Cube = new Cube(f(points))