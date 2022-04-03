import scala.scalajs.js
import js.annotation.JSExportTopLevel
import mandelbrot.Mandelbrot._
import p5._

@JSExportTopLevel("Mandelbrot")
class Mandelbrot() extends js.Object {
  val sketch: js.Function1[Sketch, Unit] = { s =>
    import s._

    setup = () =>
      createCanvas(800, 600)
      pixelDensity(1)

    draw = () =>
      background(180)
      loadPixels()

      for (x <- 0 to width.toInt; y <- 0 to height.toInt) {

        val a = map(x, 0, width, -2, 1)
        val b = map(y, 0, height, -1, 1)

        mandelbrot(a, b)
          .foreach { i =>
            val brightnes = map(i, 0, MAX_ITERATIONS, 51, 255).toInt
            val pix = (x + y * width).toInt * 4
            pixels(pix + 0) = brightnes
            pixels(pix + 1) = brightnes
            pixels(pix + 2) = brightnes
            pixels(pix + 3) = 255
          }
      }

      updatePixels()
      noLoop()
  }
}
