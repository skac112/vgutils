package com.github.skac112.vgutils

case class LineSec(p1: Point, p2: Point) {

  /**
   * straight line of this line section.
   */
  lazy val line = {
    val (a, b, c) = MathUtils.line2p(p1, p2)
    Line(a, b, c)
  }

  /**
    * Length of a line section.
    */
  lazy val len = vector.modulus

  /**
    * Square of a length of a line section.
    * @param other
    * @return
    */
  lazy val len2 = vector.modulus2

  /**
   * Finds a point of intersection (if any) od this line section with another
   * line section.
   */
  def i(other: LineSec): Option[Point] = {
      val l1 = line
      val l2 = other.line
      if (!l1.isParallelTo(l2)) {
        // point of intersection of lines corresponding to line sections.
        // It counts only if it lies on both line sections.
        val p_i = MathUtils.linearSys2(l1.a, l1.b, l1.c, l2.a, l2.b, l2.c)
        // checking if point of intersection lies on both line sections. To
        // avoid rounding errors one could use collinearity of intersection
        // points with both sections and check only distances
        if ((p_i - p1).modulus2 <= len2 && (p_i - p2).modulus2 <= len2 &&
         (p_i - other.p1).modulus2 <= other.len2 && (p_i - other.p2).modulus2 <= other.len2) {
          Some(p_i)
        }
        else {
          None
        }
      }
      else {
        None
      }
  }

  lazy val vector = p2 - p1

  lazy val slope: Angle = vector.angle
}
