package rubikscube
import scala.scalajs.js
import js.JSConverters.JSRichIterableOnce
import js.annotation.JSExportTopLevel
import p5._
import P5VectorExt._
import matrix.P5Matrix

@JSExportTopLevel("RubiksCube")
class RubiksCube() extends js.Object:
  val sketch: js.Function1[Sketch, Unit] = s =>
    import s._
    var cube = solved
    setup = () =>
      createCanvas(600, 600, WEBGL)
      //applyMatrix()

    draw = () =>
      rotateX(.45)
      rotateY(-.25)
      // rotateX(millis() / 11111);
      // rotateY(millis() / 22222);
      // rotateZ(millis() / 33333);
      background(240)
      scale(60)
      cube.cubies.foreach(_.draw(s))

    keyPressed = event =>
      if event.key == "F" then
        cube = cube.rotateZ(true, 1)
      if event.key == "f" then
        cube = cube.rotateZ(false, 1)
      if event.key == "B" then
        cube = cube.rotateZ(false, -1)
      if event.key == "b" then
        cube = cube.rotateZ(true, -1)

      if event.key == "R" then
        cube = cube.rotateX(true, 1)
      if event.key == "r" then
        cube = cube.rotateX(false, 1)
      if event.key == "L" then
        cube = cube.rotateX(false, -1)
      if event.key == "l" then
        cube = cube.rotateX(true, -1)

      if event.key == "U" then
        cube = cube.rotateY(true, -1)
      if event.key == "u" then
        cube = cube.rotateY(false, -1)
      if event.key == "D" then
        cube = cube.rotateY(false, 1)
      if event.key == "d" then
        cube = cube.rotateY(true, 1)
      end if
      false

case class Kub(cubies: Seq[Cubie]):
  def rotateX(clockwise: Boolean, i: Int): Kub =
    Kub(cubies
      .map(qb => {
        if qb.pos.x == i then
          qb.rotateX(clockwise)
        else qb
      }))

  def rotateY(clockwise: Boolean, i: Int): Kub =
    Kub(cubies
      .map(qb => {
        if qb.pos.y == i then
          qb.rotateY(clockwise)
        else qb
      }))
  def rotateZ(clockwise: Boolean, i: Int): Kub =
    Kub(cubies
      .map(qb => {
        if qb.pos.z == i then
          qb.rotateZ(clockwise)
        else qb
      }))

case class Cubie(pos: P5Matrix, faces: Seq[Face] = Seq()):
  val ninetyDegrees = Math.PI / 2
  def draw(s: Sketch) =
    import s._
    fill(255)
    stroke(0)
    strokeWeight(.1)
    push()

    applyMatrix(pos.forApplyMatrix().toJSArray)
    faces.foreach(_.draw(s))

    pop()

  def rotateX(clockwise: Boolean): Cubie =
    val angle = if clockwise then ninetyDegrees else -ninetyDegrees
    val newMatrix = new P5Matrix
    newMatrix.rotateX(angle)
    newMatrix.translate(pos.x, pos.y, pos.z)

    val m = new P5Matrix
    m.translate(newMatrix.x, newMatrix.y, newMatrix.z)

    val newFaces = faces.map { f =>
      f.copy(direction = f.direction.rotateX(clockwise))
    }

    copy(pos = m, faces = newFaces)

  def rotateY(clockwise: Boolean): Cubie =
    val angle = if clockwise then ninetyDegrees else -ninetyDegrees
    val newMatrix = new P5Matrix
    newMatrix.rotateY(angle)
    newMatrix.translate(pos.x, pos.y, pos.z)

    val m = new P5Matrix
    m.translate(newMatrix.x, newMatrix.y, newMatrix.z)

    val newFaces = faces.map { f =>
      f.copy(direction = f.direction.rotateY(clockwise))
    }

    copy(pos = m, faces = newFaces)

  def rotateZ(clockwise: Boolean): Cubie =
    val angle = if clockwise then ninetyDegrees else -ninetyDegrees
    val newMatrix = new P5Matrix
    newMatrix.rotateZ(angle)
    newMatrix.translate(pos.x, pos.y, pos.z)

    val m = new P5Matrix
    m.translate(newMatrix.x, newMatrix.y, newMatrix.z)

    val newFaces = faces.map { f =>
      f.copy(direction = f.direction.rotateZ(clockwise))
    }

    copy(pos = m, faces = newFaces)

object Cubie:
  def apply(vec: P5Vector, faces: Seq[Face]): Cubie =
    val m = new P5Matrix()
    m.translate(vec.x, vec.y, vec.z)
    Cubie(m, faces)
 
case class Face(colour: Colour, direction: Direction):
  def draw(s: Sketch) =
    import s._
    push()

    fill(colour.code)

    val faceSides = .8
    val offset = .5
    direction match {
      case Left =>
        translate(-offset, 0, 0)
        box(-.08, faceSides, faceSides)
      case Right => 
        translate(offset, 0, 0)
        box(.08, faceSides, faceSides)
      case Top =>
        translate(0, -offset, 0)
        box(faceSides, -.08, faceSides)
      case Bottom => 
        translate(0, offset, 0)
        box(faceSides, .08, faceSides)
      case Front =>
        translate(0, 0, offset)
        box(faceSides, faceSides, .08)
      case Back => 
        translate(0, 0, -offset)
        box(faceSides, faceSides, -.08)
    }

    strokeWeight(3)
    stroke(colour.code)
    pop()

sealed trait Colour(val code: String)
case object Green extends Colour("#009b48")
case object White extends Colour("#ffffff")
case object Red extends Colour("#b71234")
case object Yellow extends Colour("#ffd500")
case object Blue extends Colour("#0046ad")
case object Orange extends Colour("#ff5800")

sealed trait Direction:
  def rotateX(clockwise: Boolean): Direction
  def rotateY(clockwise: Boolean): Direction
  def rotateZ(clockwise: Boolean): Direction

case object Front extends Direction:
  def rotateX(clockwise: Boolean): Direction = if clockwise then Top else Bottom
  def rotateY(clockwise: Boolean): Direction = if clockwise then Right else Left
  def rotateZ(clockwise: Boolean): Direction = Front
case object Back extends Direction:
  def rotateX(clockwise: Boolean): Direction = Front.rotateX(!clockwise)
  def rotateY(clockwise: Boolean): Direction = Front.rotateY(!clockwise)
  def rotateZ(clockwise: Boolean): Direction = Back
case object Bottom extends Direction:
  def rotateX(clockwise: Boolean): Direction = if clockwise then Front else Back
  def rotateY(clockwise: Boolean): Direction = Bottom
  def rotateZ(clockwise: Boolean): Direction = if clockwise then Left else Right
case object Top extends Direction:
  def rotateX(clockwise: Boolean): Direction = Bottom.rotateX(!clockwise)
  def rotateY(clockwise: Boolean): Direction = Top
  def rotateZ(clockwise: Boolean): Direction = Bottom.rotateZ(!clockwise)
case object Right extends Direction: 
  def rotateX(clockwise: Boolean): Direction = Right
  def rotateY(clockwise: Boolean): Direction = if clockwise then Back else Front
  def rotateZ(clockwise: Boolean): Direction = if clockwise then Bottom else Top
case object Left extends Direction:
  def rotateX(clockwise: Boolean): Direction = Left
  def rotateY(clockwise: Boolean): Direction = Right.rotateY(!clockwise)
  def rotateZ(clockwise: Boolean): Direction = Right.rotateZ(!clockwise)

val corners =
  Seq(
    Cubie(
      new P5Vector(-1, -1, -1),
      Seq(Face(White, Top), Face(Orange, Left), Face(Blue, Back))
    ),
    Cubie(
      new P5Vector(1, -1, -1),
      Seq(
        Face(White, Top),
        Face(Red, Right),
        Face(Blue, Back)
      )
    ),
    Cubie(
      new P5Vector(-1, 1, -1),
      Seq(
        Face(Yellow, Bottom),
        Face(Orange, Left),
        Face(Blue, Back)
      )
    ),
    Cubie(
      new P5Vector(-1, -1, 1),
      Seq(
        Face(White, Top),
        Face(Orange, Left),
        Face(Green, Front)
      )
    ),
    Cubie(
      new P5Vector(1, 1, -1),
      Seq(
        Face(Red, Right),
        Face(Yellow, Bottom),
        Face(Blue, Back)
      )
    ),
    Cubie(
      new P5Vector(1, -1, 1),
      Seq(
        Face(White, Top),
        Face(Green, Front),
        Face(Red, Right)
      )
    ),
    Cubie(
      new P5Vector(-1, 1, 1),
      Seq(
        Face(Orange, Left),
        Face(Yellow, Bottom),
        Face(Green, Front)
      )
    ),
    Cubie(
      new P5Vector(1, 1, 1),
      Seq(Face(Green, Front), Face(Yellow, Bottom), Face(Red, Right))
    )
  )
val debugOne =
  Seq(
    Cubie(
      new P5Vector(1, 1, 1),
      Seq(Face(Green, Front), Face(Yellow, Bottom), Face(Red, Right))
    )
  )

val mids =
  Seq(
    Cubie(
      new P5Vector(1, 1, 0),
      Seq(Face(Red, Right), Face(Yellow, Bottom))
    ),
    Cubie(
      new P5Vector(1, 0, 1),
      Seq(Face(Red, Right), Face(Green, Front))
    ),
    Cubie(
      new P5Vector(0, 1, 1),
      Seq(Face(Yellow, Bottom), Face(Green, Front))
    ),
    Cubie(
      new P5Vector(-1, -1, 0),
      Seq(Face(Orange, Left), Face(White, Top))
    ),
    Cubie(
      new P5Vector(-1, 0, -1),
      Seq(Face(Orange, Left), Face(Blue, Back))
    ),
    Cubie(
      new P5Vector(0, -1, -1),
      Seq(Face(White, Top), Face(Blue, Back))
    ),
    Cubie(
      new P5Vector(0, 1, -1),
      Seq(Face(Yellow, Bottom), Face(Blue, Back))
    ),
    Cubie(
      new P5Vector(0, -1, 1),
      Seq(Face(White, Top), Face(Green, Front))
    ),
    Cubie(
      new P5Vector(1, 0, -1),
      Seq(Face(Red, Right), Face(Blue, Back))
    ),
    Cubie(
      new P5Vector(-1, 0, 1),
      Seq(Face(Orange, Left), Face(Green, Front))
    ),
    Cubie(
      new P5Vector(1, -1, 0),
      Seq(Face(White, Top), Face(Red, Right))
    ),
    Cubie(
      new P5Vector(-1, 1, 0),
      Seq(Face(Orange, Left), Face(Yellow, Bottom))
    )
  )

val centers =
  Seq(
    Cubie(
      new P5Vector(1, 0, 0),
      Seq(Face(Red, Right))
    ),
    Cubie(
      new P5Vector(-1, 0, 0),
      Seq(Face(Orange, Left))
    ),
    Cubie(
      new P5Vector(0, 1, 0),
      Seq(Face(Yellow, Bottom))
    ),
    Cubie(
      new P5Vector(0, -1, 0),
      Seq(Face(White, Top))
    ),
    Cubie(
      new P5Vector(0, 0, -1),
      Seq(Face(Blue, Back))
    ),
    Cubie(
      new P5Vector(0, 0, 1),
      Seq(Face(Green, Front))
    )
  )

val solved = Kub(corners ++ mids ++ centers)
//val solved = Kub(debugOne)
//val solved = Kub(corners)