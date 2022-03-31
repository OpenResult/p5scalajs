import scala.scalajs.js
import scala.scalajs.js.|
import scala.scalajs.js.annotation.JSName
import scala.scalajs.js.annotation.JSGlobal

@js.native
trait Sketch extends js.Object {
  var setup: js.Function0[Unit]
  var draw: js.Function0[Unit]
  var CENTER: String = js.native
  var HALF_PI: js.Any = js.native
  var PI: js.Any = js.native
  var QUARTER_PI: js.Any = js.native
  var TAU: js.Any = js.native
  var TWO_PI: js.Any = js.native
  var mouseX: Double = js.native
  var mouseY: Double = js.native
  var pixels: js.Array[Int] = js.native
  def map(
      value: Double,
      start1: Double,
      stop1: Double,
      start2: Double,
      stop2: Double
  ): Double = js.native
  def translate(x: Double, y: Double): js.Any = js.native
  def dist(x1: Double, y1: Double, x2: Double, y2: Double): Double = js.native
  def pixelDensity(d: Double): js.Any = js.native
  def loadPixels(): js.Any = js.native
  def updatePixels(): js.Any = js.native
  def push(): js.Any = js.native
  def pop(): js.Any = js.native
  def ellipse(a: Double, b: Double, c: Double, d: Double): js.Any = js.native
  def line(x1: Double, y1: Double, x2: Double, y2: Double): js.Any = js.native
  def point(x: Double, y: Double): js.Any = js.native
  def text(s: String, x: Double, y: Double): js.Any = js.native
  def textAlign(a: String, b: String): js.Any = js.native
  def textSize(size: Double): js.Any | Double = js.native
  def background(c: Double): js.Any = js.native
  def createCanvas(w: Double, h: Double, renderer: String = ""): P5Renderer =
    js.native
  def createVector(x: Double, y: Double): js.Dynamic =
    js.native
  def noLoop(): js.Any = js.native
  def circle(x: Double, y: Double, d: Double): js.Any = js.native
  def fill(
      v1: Double | js.Array[js.Any] | String,
      v2: Double = 1.0,
      v3: Double = 1.0,
      a: Double = 1.0
  ): Unit = js.native
  def height: Double = js.native
  def width: Double = js.native
  def triangle(
      x1: Double,
      y1: Double,
      x2: Double,
      y2: Double,
      x3: Double,
      y3: Double
  ): js.Any = js.native
  def rect(
      x: Double,
      y: Double,
      w: Double,
      h: Double,
      tl: Double = 1.0,
      tr: Double = 1.0,
      br: Double = 1.0,
      bl: Double = 1.0
  ): js.Any = js.native
  def frameRate(fps: Double = 1.0): Double = js.native
  def createVector(x: Double = 0, y: Double = 0, z: Double = 0): P5Vector = js.native
  def stroke(a: Double | String, b: Double = 0): js.Any = js.native
  def strokeWeight(a: Double = 0): js.Any = js.native
}

@js.native
@JSGlobal("p5.Vector")
class P5Vector(val x: Double, val y: Double, val z: Double = 0)
    extends js.Object {
  def dist(other: P5Vector): Double = js.native
  def angleBetween(other: P5Vector): Double = js.native
  def mag(): Double = js.native
  @deprecated("Use immutable withMag")
  def setMag(n: Double): P5Vector = js.native
  def copy(): P5Vector = js.native
}

@js.native
@JSGlobal("p5.Vector")
object P5Vector extends js.Object {
  def random2D(): P5Vector = js.native
  def add(v1: P5Vector, v2: P5Vector): P5Vector = js.native
  def sub(v1: P5Vector, v2: P5Vector): P5Vector = js.native
  def mult(v: P5Vector, n: Double): P5Vector = js.native
  def div(v: P5Vector, n: Double): P5Vector = js.native
}

object P5VectorExt {
  extension (v: P5Vector)
    def +(other: P5Vector) = P5Vector.add(v, other)
    def -(other: P5Vector) = P5Vector.sub(v, other)
    def *(n: Double) = P5Vector.mult(v, n)
    def /(n: Double) = P5Vector.div(v, n)
    def negX() =
      new P5Vector(v.x * -1, v.y, v.z)
    def negY() =
      new P5Vector(v.x, v.y * -1, v.z)
    def neg() = v * -1
    def withMag(mag: Double) =
      v.copy().setMag(mag)
    def withMag(f: Double => Double) =
      v.copy().setMag(f(v.mag()))
}

@js.native
@JSGlobal("p5.Renderer")
class P5Renderer extends js.Object:
  def mousePressed(f: js.Function): P5Renderer = js.native
