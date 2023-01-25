package matrix

import utest._
import matrix.P5Matrix

object P5MatrixSuite extends TestSuite:

  val tests = Tests {
    test("new matrix") {
      val result = new P5Matrix()
      
      result ==> P5Matrix(
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1
      )
    }

    test("transform") {
      val result = new P5Matrix()

      result.translate(1, 0, -1)

      result ==> P5Matrix(
        1, 0, 0,  1,
        0, 1, 0,  0,
        0, 0, 1, -1,
        0, 0, 0,  1)
    }

    test("rotatex 360 degrees") {
      val result = new P5Matrix()

      result.rotateX(Math.PI * 2)
      result.translate(1, 0, -1)

      result.text() ==> """
        |1, 0, 0, 1,
        |0, 1, 0, 0,
        |0, 0, 1, -1,
        |0, 0, 0, 1""".stripMargin

    }
    test("rotate z 90 degrees") {
      val result = new P5Matrix()

      val pos = (result.x, result.y, result.z)
      pos ==> (0, 0, 0)

      result.rotateZ(Math.PI / 2)
      result.translate(1, 1, 0)

      val newPos = (result.x, result.y, result.z)
      newPos ==> (-1, 1, 0)
    }
    test("rotate z 180 degrees") {
      val result = new P5Matrix()

      val pos = (result.x, result.y, result.z)
      pos ==> (0, 0, 0)

      result.rotateZ(Math.PI)
      result.translate(1, 1, 0)

      val newPos = (result.x, result.y, result.z)
      newPos ==> (-1, -1, 0)
    }
  }
