import scala.scalajs.js
import js.timers.setTimeout
import js.annotation.JSExportTopLevel
import minesweeper._
import p5._

@JSExportTopLevel("Minesweeper")
class Minesweeper(statusCallback: js.Function1[String, Unit]) extends js.Object {
  val sketch: js.Function1[Sketch, Unit] = { s =>
    import s._
    val rows = 10
    val cols = 10
    val cellSize = 40
    val mines = 12
    val timeout: (Double, (() => Unit)) => Unit = (d, f) =>
      setTimeout(d)(f)
    var board: Board = new Board(rows, cols, cellSize, timeout)
        .withMines(mines)
        .statusCallback(statusCallback)

    setup = () =>
      val cnvs = createCanvas(board.cols * cellSize, board.rows * cellSize)
      cnvs.mousePressed { () =>
        board = board.click(mouseX, mouseY)
        if board.ended then noLoop()
      }
      Board.gameLoop(board)

    draw = () =>
      background(232)
      board.forCells { c =>
        val x = c.x(cellSize)
        val y = c.y(cellSize)
        if c.revealed then
          stroke(100)
          rect(x, y, cellSize, cellSize)
          
          if c.mine then
            push()
            fill("red")
            circle(x + cellSize / 2, y + cellSize / 2, cellSize * 0.8)
            pop()
          else
            text(
              "" + board.neighbouringMines(c),
              x + cellSize / 2,
              y + cellSize / 2
            )
        else
          stroke(200)
          rect(x, y, cellSize, cellSize)
      }
  }
}
