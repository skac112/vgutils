package com.github.skac112.vgutils.transform.linear

import com.github.skac112.vgutils._

import scala.math._

/**
 * @author slawek
 */
case class Rotation(angle: Double) extends GenericLinear(
  a = cos(angle),
  b = -sin(angle),
  c = sin(angle),
  d = cos(angle)) {
  lazy val co = cos(angle)
  lazy val si = sin(angle)

  /**
   * Using this construction a rotation can be specified in this way for example:
   * 30 deg
   */
  def deg = this.copy(angle = scala.math.Pi / 180.0 * this.angle)
  
  /**
   * Using this construction a rotation can be specified in this way for example:
   * 2.5 rad
   */
  def rad = this.copy(angle = this.angle)
}
