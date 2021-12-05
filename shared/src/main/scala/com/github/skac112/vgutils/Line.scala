package com.github.skac112.vgutils

/**
 * Straight line parameterized by equation: ax + by + c = 0.
 */
case class Line(a: Double, b: Double, c: Double) {
  lazy val a2b2 = a*a + b*b
  lazy val sqrta2b2 = math.sqrt(a2b2)

  def isParallelTo(other: Line) = (a*other.b - b*other.a) about 0

  /**
   * Finds a line parallel to this line and passing through a given point
   * @param pt
   */
  def parallelBy(pt: Point) = Line(a, b, -a*pt.x - b*pt.y)

  /**
   * Finds a line perpendicular to this line and passing through a given point
   * @param pt
   * @return
   */
  def perpendicularBy(pt: Point) = Line(b, -a, -b*pt.x + a*pt.y)

  def distToPt(pt: Point) = math.abs(pt.x*a + pt.y*b + c) / sqrta2b2

  /**
   * Projection of point pt - it is an intersection of this line with line going to given point pt and perpendicular
   * to this line
   */
  def ptProj(pt: Point) = {
    lazy val bxay = b*pt.x - a*pt.y
    Point((b*bxay - a*c) / a2b2, (-a*bxay - b*c)/ a2b2)
  }
}
