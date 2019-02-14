package com.github.skac112.vgutils.transform

/**
 * @author slawek
 */
case class Scale(scaleX: Double, scaleY: Double) extends GenericLinear(scaleX, .0, .0, scaleY) {
  def uniform(scale: Double) = Scale(scale, scale)
}
