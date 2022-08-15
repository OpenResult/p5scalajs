package minesweeper

case class Board(
    rows: Int,
    cols: Int,
    cellSize: Int,
    timeout: (Double, () => Unit) => Unit,
    cells: Map[Position, Cell] = Map(),
    ended: Boolean = false,
    statusCallback: Option[String => Unit] = None
):
  val positions = for {
    col <- (0 until cols)
    row <- (0 until rows)
  } yield Position(col, row)

  lazy val completed: Boolean = {
    allCells.forall { c =>
      (c.revealed && !c.mine) || (!c.revealed && c.mine)
    }
  }

  def click(x: Double, y: Double): Board =
    val c = getCellByCordinates(x, y)
    if ended then this
    else if c.mine then
      val revealedCells = this.cells.mapValues(_.copy(revealed = true)).toMap
      this.copy(cells = revealedCells, ended = true)
    else
      val revealed = floodReveal(c)
      val b = this.copy(cells = this.cells ++ revealed)
      if b.completed then b.copy(ended = true)
      else b

  def floodReveal(
      c: Cell,
      acc: Map[Position, Cell] = Map()
  ): Map[Position, Cell] =
    if acc.contains(c.p) then acc
    else if neighbouringMines(c) == 0 then
      cellNeighbours(c)
        .foldLeft(acc + (c.p -> c.reveal)) { (acc, n) =>
          floodReveal(n, acc)
        }
    else acc + (c.p -> c.reveal)

  def cellNeighbours(c: Cell): Seq[Cell] =
    val r = c.p.neighbours
      .filter(p => p.col < cols && p.row < rows)
      .map(p => getCell(p))
    r

  def getCellByCordinates(x: Double, y: Double): Cell =
    getCell(Position(x.toInt / cellSize, y.toInt / cellSize))

  def getCell(p: Position): Cell =
    cells.getOrElse(p, Cell(p, false))

  def forCells(f: Cell => Unit) = positions.foreach(p => f(getCell(p)))

  def allCells: Seq[Cell] = positions.map(getCell(_))

  def neighbouringMines(c: Cell): Int = cellNeighbours(c)
    .filter(_.mine)
    .size

  def withMines(mines: Map[Position, Cell]): Board =
    this.copy(cells = this.cells ++ mines)

  def withMines(n: Int): Board =
    import scala.util.Random.shuffle
    withMines(
      shuffle(positions)
        .take(n)
        .map(p => p -> Cell(p, true))
        .toMap
    )

  def statusCallback(f: String => Unit) =
    copy(statusCallback = Some(f))

object Board {
  def gameLoop(board: => Board): Unit =
    var startTime: Long = System.currentTimeMillis()
    loop()
    def loop(): Unit = {
      val now = System.currentTimeMillis()
      val elaps = now - startTime
      val seconds = elaps / 1000
      val minutes = seconds / 60
      val s = seconds % 60
      val time = minutes + ":" + (if s < 10 then "0" + s else s)

      if board.ended && board.completed then
        board.statusCallback
          .foreach(f => f(s"Game finished successfully. Time: $time"))
      else if board.ended then
        board.statusCallback
          .foreach(f => f(s"Game ended with a boom! Time: $time"))
      else
        board.statusCallback
          .foreach(_(s"Time: $time"))

        board.timeout(333, { () =>
          loop()
        })
    }
}

case class Cell(p: Position, mine: Boolean, revealed: Boolean = false):
  def x(width: Double) = p.col * width
  def y(height: Double) = p.row * height
  def reveal = copy(revealed = true)

case class Position(col: Int, row: Int):
  def neighbours: Seq[Position] =
    val xs = for {
      c <- (-1 to 1)
      r <- (-1 to 1)
    } yield Position(col + c, row + r)
    xs.view
      .filterNot(_.col < 0)
      .filterNot(_.row < 0)
      .filterNot(p => p.col == col && p.row == row)
      .toSeq
