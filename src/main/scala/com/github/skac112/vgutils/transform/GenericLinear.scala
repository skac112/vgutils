package com.github.skac112.vgutils.transform

import com.github.skac112.vgutils.Point

/**
 * @author slawek
 */
class GenericLinear(a: Double, b: Double, c: Double, d: Double) extends Affine(a, b, c, d, .0, .0) {
  def +(pt: Point) = new Affine(a, b, c, d, pt.x, pt.y)
  def -(pt: Point) = new Affine(a, b, c, d, -pt.x, -pt.y)
  def *(factor: Double) = Linear(a * factor, b * factor, c * factor, d * factor)
  def /(factor: Double) = Linear(a / factor, b / factor, c / factor, d / factor)
  lazy val det = a * d - b * c

  /**
   * Transformacja odwrotna
   */
  override lazy val inv = Linear(d, -b, -c, a) / det
}
