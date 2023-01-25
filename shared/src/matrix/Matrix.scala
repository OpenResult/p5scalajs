package matrix

type Matrix = Seq[Seq[Double]]
type MatrixVector = Seq[Double]

object MatrixVectorExt {
  extension (mv: MatrixVector)
    def vecmult(that: MatrixVector) = mv
      .zip(that)
      .map(_ * _)
      .foldLeft(0.0)(_ + _)

    def x = mv(0)
    def y = mv(1)
    def z = mv(2)
}

object MatrixExt {
  extension (v: Matrix)
    def colVector(i: Int): MatrixVector = v.map(l => l(i))
    def rowVector(i: Int): MatrixVector = v(i)
    def mult(n: Double): Matrix =
      v.map(_.map(_ * n))
    def matmult(that: Matrix): Matrix = {
      import MatrixVectorExt._
      val l: Seq[Double] = for {
        row <- 0 until v.size
        col <- 0 until v(0).size
      } yield v.rowVector(row) vecmult that.colVector(col)
      l.grouped(that(0).size).toSeq
    }
    def projected: Matrix = {
      import MatrixVectorExt._
      val l: Seq[Double] = for {
        row <- 0 until v.size
        col <- 0 until v(0).size
      } yield {
        val rv = v.rowVector(row)
        val p = Matrix.projection(rv.z)
        p.colVector(col) vecmult rv
      }
      l.grouped(3).toSeq
    }
}
object Matrix:
  def rotationX(angle: Double): Matrix =
    import Math._
    Seq(
      Seq(1, 0, 0),
      Seq(0, cos(angle), -sin(angle)),
      Seq(0, sin(angle), cos(angle))
    )
  def rotationY(angle: Double): Matrix =
    import Math._
    Seq(
      Seq(cos(angle), 0, -sin(angle)),
      Seq(0, 1, 0),
      Seq(sin(angle), 0.0, cos(angle))
    )
  def rotationZ(angle: Double): Matrix =
    import Math._
    Seq(
      Seq(cos(angle), -sin(angle), 0),
      Seq(sin(angle), cos(angle), 0),
      Seq(0, 0, 1)
    )
  def projection(z: Double): Matrix =
    val distance = 3
    val zProj = 1 / (distance - z)
    Seq(
      Seq(zProj, 0, 0),
      Seq(0, zProj, 0),
      Seq(0, 0, 1)
    )
