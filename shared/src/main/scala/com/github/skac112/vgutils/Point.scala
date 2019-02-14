package com.github.skac112.vgutils

import scala.math._
import com.github.skac112.vgutils.transform._

object Point {
  var smallDist = 1E-6
  def versor(ang: Angle) = Point(ang.x, ang.y)
  def withAngle(dir: Angle, length: Double) = Point(dir.x * length, dir.y * length)

  // def withDir(dir: Double, length: Double) = {
  //   val angle = Angle(dir)
  //   Point(angle.x * length, angle.y * length)
  // }
}

/**
 * @author slawek
 */
case class Point(x: Double, y: Double) {
  import Point._
  lazy val modulus = sqrt(x * x + y * y)
  lazy val modulus2 = x * x + y * y
  lazy val inv = Point(-x, -y)
  lazy val unary_- = inv
  lazy val versor = Point(x / modulus, y / modulus)
  lazy val angle = Angle(x, y)
  def +(Other: Point) = Point(x + Other.x, y + Other.y)
  def -(Other: Point) = Point(x - Other.x, y - Other.y)
  def *(Factor: Double) = Point(x * Factor, y * Factor)
  def dist(Other: Point) = (this - Other).modulus
  // iloczyn skalarny
  def *(Other: Point) = x * Other.x + y * Other.y
  // iloczyn wektorowy (skalar bedacy dlugoscia "ze znakiem" wektora iloczynu w osi "z")
  def **(Other: Point) = x * Other.y - y * Other.x
  def /(Factor: Double) = Point(x / Factor, y / Factor)
  def closeTo(other: Point) = (this - other).modulus2 < smallDist
  def rot(Deg: Double) = (Rotation(Deg)) transPt this
  def r(deg: Double) = rot(deg)
//  def rot(Deg: Double) = Point(x * cos(Deg) - y * sin(Deg), x * sin(Deg) + y * cos(Deg))
  def rot(Deg: Double, Pivot: Point): Point = Pivot + ((this - Pivot) rot Deg)
  def rot(Rot: Rotation): Point = rot(Rot.angle, Rot.pivot)

  /**
   * Kat skierowany ze znakiem pomiedzy wektorem opisywanym przez ten punkt i
   * wektorem opisywanym przez drugi punkt
   */
  def angSig(other: Point): Double = signum(x * other.y - y * other.x) *
   acos(this * other / modulus / other.modulus)

  /**
   * Kat skierowany pomiedzy wektorem opisywanym przez ten punkt i wektorem opisywanym przez drugi punkt
   */
  def ang(other: Point): Angle = Angle(angSig(other))

  def map(mapF: ((Double, Double)) => (Double, Double)): Point = {
    val (new_x, new_y) = mapF((x, y))
    Point(new_x, new_y)
  }

  def flatMap(fmapF: ((Double, Double)) => Point): Point = fmapF((x, y))
}
