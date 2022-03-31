import scala.scalajs.js
import js.annotation.JSExportTopLevel
import js.annotation.JSExport
import org.scalajs.dom.raw._

import org.scalajs.dom

import upickle.default._
import shared.Protocol
import P5VectorExt._
import scala.util.Random

@JSExportTopLevel("VectorExamples")
class VectorExamples() extends js.Object {
  val sketch: js.Function1[Sketch, Unit] = { s =>
    import s._

    setup = () =>
      createCanvas(400, 400)
      background(222)
      frameRate(1)

    draw = () =>
      val rnd1 = P5Vector.random2D()
      val rnd2 = P5Vector.random2D()
      val added = rnd1 + rnd2
      val subtracted = added - rnd2
      val rndNumber = Random.between(100, 200)
      val scaled = subtracted * rndNumber

      translate(width / 2, height / 2)
      strokeWeight(4)
      stroke(0, 50)
      line(0, 0, scaled.x, scaled.y)
  }
}

@JSExportTopLevel("VectorExamples02")
class VectorExamples02() extends js.Object {
  val sketch: js.Function1[Sketch, Unit] = { s =>
    import s._

    setup = () =>
      frameRate(1)
      createCanvas(400, 400)
      background(200)

    draw = () =>
      val v1 = new P5Vector(width / 2, height / 2)
      val mouse = new P5Vector(mouseX, mouseY)
      val v = mouse - v1

      translate(width / 2, height / 2)
      strokeWeight(4)
      stroke(0, 50)
      line(0, 0, v.x, v.y)
  }
}

@JSExportTopLevel("VectorExamples03")
class VectorExamples03() extends js.Object {
  val sketch: js.Function1[Sketch, Unit] = { s =>
    import s._

    var pos: P5Vector = null
    var vel = new P5Vector(1.1, 1.1)
    var acc = new P5Vector(1.1, 1.1)

    setup = () =>
      createCanvas(400, 400)
      frameRate(30)
      background(222)

      pos = new P5Vector(width / 2, height / 2)

    draw = () =>
      background(250)
      val mouse = new P5Vector(mouseX, mouseY)
      acc = (mouse - pos).withMag(0.1)
      vel = vel + acc
      pos = pos + vel

      // strokeWeight(4)
      // stroke("red")
      fill("blue")
      circle(pos.x, pos.y, 25)
  }
}
@JSExportTopLevel("SineCosine")
class SineCosine() extends js.Object {
  val sketch: js.Function1[Sketch, Unit] = { s =>
    import s._
    import scala.math._
    
    var pos: P5Vector = null
    var theta = 0.0

    setup = () =>
      createCanvas(1000, 200)
      frameRate(15)
      

    draw = () =>
      background(222)
      val xs = (0 to width.toInt)
      xs.foreach {x =>
        val sinX = sin(theta) * 50
        theta = theta + 0.05
        point(x, sinX + 100)
      }
  }
}