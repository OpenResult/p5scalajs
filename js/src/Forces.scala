import scala.scalajs.js
import js.annotation.JSExportTopLevel
import matrix._
import matrix.MatrixVector
import matrix.MatrixVectorExt._
import matrix.Matrix
import matrix.Matrix._
import matrix.MatrixExt._

@JSExportTopLevel("Forces")
class Forces() extends js.Object {
  val sketch: js.Function1[Sketch, Unit] = { s =>
    import s._
    import P5VectorExt._

    val center = P5Vector(400, 400)
    var balls = Seq(
      ForceBall(P5Vector(360, 360), P5Vector(0, 0), 10)(s),
      ForceBall(P5Vector(40, 40), P5Vector(0, 0), 20)(s)
    )

    setup = () => createCanvas(800, 800)

    draw = () =>
      background(200)
      val mouse = P5Vector(mouseX, mouseY)
      fill(0)
      ellipse(center.x, center.y, 64, 64)
      fill(100)
      ellipse(mouse.x, mouse.y, 64, 64)
      balls = balls
        .map { ball =>

          val mMag = map(mouse.dist(ball.pos), 0, 800, 0.2, 0.01)
          val toMouseAcc = (mouse - ball.pos).withMag(mMag)

          val cMag = map(center.dist(ball.pos), 0, 800, 0.2, 0.01)
          val toCenterForce = (center - ball.pos).withMag(cMag)

          ball
            .applyForce(toMouseAcc)
            .applyForce(toCenterForce)
            .draw()
        }
  }
}
case class ForceBall(pos: P5Vector, vel: P5Vector, mass: Double)(implicit
    s: Sketch
):
  import P5VectorExt._
  lazy val r = Math.sqrt(mass) * 10
  def applyForce(force: P5Vector): ForceBall =
    val f = force / mass
    copy(vel = vel + f)

  def draw(): ForceBall =
    import s._
    val newPos = pos + vel
    fill(255)
    ellipse(pos.x, pos.y, r, r)
    copy(pos = newPos)
      .edges()

  def edges(): ForceBall =
    if pos.y >= s.height then copy(vel = vel.negY())
    else if pos.x >= s.width then copy(vel = vel.negX())
    else if pos.y <= 0 then copy(vel = vel.negY())
    else if pos.x <= 0 then copy(vel = vel.negX())
    else this
