package com.github.skac112.vgutils

/**
 * Straight line parameterized by equation: ax + by + c = 0.
 */
case class Line(a: Double, b: Double, c: Double) {
  def isParallelTo(other: Line) = a*other.b - b*other.a != 0

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
}
