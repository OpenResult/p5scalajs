package matrix

// https://www.youtube.com/playlist?list=PLTd6ceoshprfZs1VIzGHDt-MYgVewC5tc

case class P5Matrix(
  var m00: Double = 1,  var m01: Double = 0,  var m02: Double = 0,  var m03: Double = 0,
  var m10: Double = 0,  var m11: Double = 1,  var m12: Double = 0,  var m13: Double = 0,
  var m20: Double = 0,  var m21: Double = 0,  var m22: Double = 1,  var m23: Double = 0,
  var m30: Double = 0,  var m31: Double = 0,  var m32: Double = 0,  var m33: Double = 1) {
    import Math.{sin, cos, round}
    def x = round(m03)
    def y = round(m13)
    def z = round(m23)
    def xyz = s"x:$x, y:$y, z:$z"
    def forApplyMatrix() = Seq(
      m00, m10, m20, m30,
      m01, m11, m21, m31,
      m02, m12, m22, m32,
      m03, m13, m23, m33
    )
    def forApplyMatrix2() = Seq(
      m00, m01, m02, m03,
      m10, m11, m12, m13,
      m20, m21, m22, m23,
      m30, m31, m32, m33
    )
    def translate(tx: Long, ty: Long, tz: Long): Unit = translate(tx.toDouble, ty.toDouble, tz.toDouble)
    def translate(tx: Double, ty: Double, tz: Double): Unit = {
      m03 = m03 + tx*m00 + ty*m01 + tz*m02;
      m13 = m13 + tx*m10 + ty*m11 + tz*m12;
      m23 = m23 + tx*m20 + ty*m21 + tz*m22;
      m33 = m33 + tx*m30 + ty*m31 + tz*m32;
    }

    def mult(vector: (Double, Double, Double)): (Double, Double, Double) =
      val (x, y, z) = vector
      (m00*x + m01*y + m02*z + m03,
              m10*x + m11*y + m12*z + m13,
              m20*x + m21*y + m22*z + m23)

    def rotateX(angle: Double) = {
      val c = cos(angle)
      val s = sin(angle)
      apply(1, 0, 0, 0,  0, c, -s, 0,  0, s, c, 0,  0, 0, 0, 1)
    }


    def rotateY(angle: Double) = {
      val c = cos(angle)
      val s = sin(angle)
      apply(c, 0, s, 0,  0, 1, 0, 0,  -s, 0, c, 0,  0, 0, 0, 1)
    }


    def rotateZ(angle: Double) = {
      val c = cos(angle)
      val s = sin(angle)
      apply(c, -s, 0, 0,  s, c, 0, 0,  0, 0, 1, 0,  0, 0, 0, 1)
    }

    def apply(n00: Double, n01: Double, n02: Double, n03: Double,
                  n10: Double, n11: Double, n12: Double, n13: Double,
                  n20: Double, n21: Double, n22: Double, n23: Double,
                  n30: Double, n31: Double, n32: Double, n33: Double) =

      val r00 = m00*n00 + m01*n10 + m02*n20 + m03*n30;
      val r01 = m00*n01 + m01*n11 + m02*n21 + m03*n31;
      val r02 = m00*n02 + m01*n12 + m02*n22 + m03*n32;
      val r03 = m00*n03 + m01*n13 + m02*n23 + m03*n33;

      val r10 = m10*n00 + m11*n10 + m12*n20 + m13*n30;
      val r11 = m10*n01 + m11*n11 + m12*n21 + m13*n31;
      val r12 = m10*n02 + m11*n12 + m12*n22 + m13*n32;
      val r13 = m10*n03 + m11*n13 + m12*n23 + m13*n33;

      val r20 = m20*n00 + m21*n10 + m22*n20 + m23*n30;
      val r21 = m20*n01 + m21*n11 + m22*n21 + m23*n31;
      val r22 = m20*n02 + m21*n12 + m22*n22 + m23*n32;
      val r23 = m20*n03 + m21*n13 + m22*n23 + m23*n33;

      val r30 = m30*n00 + m31*n10 + m32*n20 + m33*n30;
      val r31 = m30*n01 + m31*n11 + m32*n21 + m33*n31;
      val r32 = m30*n02 + m31*n12 + m32*n22 + m33*n32;
      val r33 = m30*n03 + m31*n13 + m32*n23 + m33*n33;

      m00 = r00; m01 = r01; m02 = r02; m03 = r03;
      m10 = r10; m11 = r11; m12 = r12; m13 = r13;
      m20 = r20; m21 = r21; m22 = r22; m23 = r23;
      m30 = r30; m31 = r31; m32 = r32; m33 = r33;
    


    def prepend(n: P5Matrix): Unit = prepend(
      n.m00, n.m01, n.m02, n.m03,
      n.m10, n.m11, n.m12, n.m13,
      n.m20, n.m21, n.m22, n.m23,
      n.m30, n.m31, n.m32, n.m33)

    def prepend(n00: Double, n01: Double, n02: Double, n03: Double,
                      n10: Double, n11: Double, n12: Double, n13: Double,
                      n20: Double, n21: Double, n22: Double, n23: Double,
                      n30: Double, n31: Double, n32: Double, n33: Double): Unit = {

      val r00 = n00*m00 + n01*m10 + n02*m20 + n03*m30
      val r01 = n00*m01 + n01*m11 + n02*m21 + n03*m31
      val r02 = n00*m02 + n01*m12 + n02*m22 + n03*m32
      val r03 = n00*m03 + n01*m13 + n02*m23 + n03*m33

      val r10 = n10*m00 + n11*m10 + n12*m20 + n13*m30;
      val r11 = n10*m01 + n11*m11 + n12*m21 + n13*m31;
      val r12 = n10*m02 + n11*m12 + n12*m22 + n13*m32;
      val r13 = n10*m03 + n11*m13 + n12*m23 + n13*m33;

      val r20 = n20*m00 + n21*m10 + n22*m20 + n23*m30;
      val r21 = n20*m01 + n21*m11 + n22*m21 + n23*m31;
      val r22 = n20*m02 + n21*m12 + n22*m22 + n23*m32;
      val r23 = n20*m03 + n21*m13 + n22*m23 + n23*m33;

      val r30 = n30*m00 + n31*m10 + n32*m20 + n33*m30;
      val r31 = n30*m01 + n31*m11 + n32*m21 + n33*m31;
      val r32 = n30*m02 + n31*m12 + n32*m22 + n33*m32;
      val r33 = n30*m03 + n31*m13 + n32*m23 + n33*m33;

      m00 = r00; m01 = r01; m02 = r02; m03 = r03;
      m10 = r10; m11 = r11; m12 = r12; m13 = r13;
      m20 = r20; m21 = r21; m22 = r22; m23 = r23;
      m30 = r30; m31 = r31; m32 = r32; m33 = r33;
    }

    def text() =
      s"""
        |${Math.round(m00)}, ${Math.round(m01)}, ${Math.round(m02)}, ${Math.round(m03)},
        |${Math.round(m10)}, ${Math.round(m11)}, ${Math.round(m12)}, ${Math.round(m13)},
        |${Math.round(m20)}, ${Math.round(m21)}, ${Math.round(m22)}, ${Math.round(m23)},
        |${Math.round(m30)}, ${Math.round(m31)}, ${Math.round(m32)}, ${Math.round(m33)}""".stripMargin
}


