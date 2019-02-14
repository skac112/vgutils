package com.github.skac112.vgutils.transform

import com.github.skac112.vgutils._
import scala.math._

/**
 * @author slawek
 */
case class Rotation(angle: Double, pivot: Point = ori) extends Affine(a = cos(angle),
  b = -sin(angle),
  c = sin(angle),
  d = cos(angle),
  e = pivot.x * (1 - cos(angle) + sin(angle)),
  f = pivot.y * (1 - sin(angle) - cos(angle))) {
  val co = cos(angle)
  val si = sin(angle)
  def around(NewPivot: Point) = this.copy(pivot = NewPivot)
  // def deg(ang: Double) = this.copy(angle = scala.math.Pi / 180.0 * ang)
  // def rad(ang: Double) = this.copy(angle = ang)
  def deg = this.copy(angle = scala.math.Pi / 180.0 * this.angle)
  
  /**
   * Dzieki takiej konstrukcji mozemy wyrazic obrot np. tak: 2 rad
   */
  def rad = this.copy(angle = this.angle)
}
