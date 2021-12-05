package com.github.skac112.vgutils.transform.linear

import com.github.skac112.vgutils.Point
import com.github.skac112.vgutils.transform.Affine

/**
 * Linear 2-d transformation, i.e. 2-d matrix.
 */
class GenericLinear(val a: Double, val b: Double, val c: Double, val d: Double) extends (Point => Point) {
  def *(factor: Double) = Linear(a * factor, b * factor, c * factor, d * factor)
  def /(factor: Double) = Linear(a / factor, b / factor, c / factor, d / factor)
  def +(pt: Point) = new Affine(a, b, c, d, pt.x, pt.y)
  def -(pt: Point) = new Affine(a, b, c, d, -pt.x, -pt.y)

  /**
   * Composition with other GenericLinear (this applied first). It is a matrix multiplication of the form:
   * this * other.
   * @param other
   * @return
   */
  def *(other: GenericLinear) = Linear(
    a * other.a + b * other.c,
    a * other.b + b * other.d,
    c * other.a + d * other.c,
    c * other.b + d * other.d)

  /**
   * Determinant
   */
  lazy val det = a * d - b * c

  /**
   * Inverse transform (matrix inverse).
   */
  lazy val inv = Linear(d, -b, -c, a) / det

  def apply(p: Point) = Point(a*p.x + b*p.y, c*p.x + d*p.y)

  def add(other: GenericLinear): GenericLinear = Linear(a + other.a, b + other.b, c + other.c, d + other.d)

  def + = add _

  def sub(other: GenericLinear): GenericLinear = Linear(a - other.a, b - other.b, c - other.c, d - other.d)

  def - = sub _

  def unitary_-(): GenericLinear = Linear(-a, -b, -c, -d)
}
