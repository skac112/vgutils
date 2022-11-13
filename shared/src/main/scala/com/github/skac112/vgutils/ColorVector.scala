package com.github.skac112.vgutils

import scala.math._

object ColorVector {
  lazy val pi_3_inv = 3.0 / Pi

  def hsla(hue: Angle, sat: Double, l: Double, a: Double = 1.0) = {
    // chroma
    val C = (1 - abs(2*l - 1.0))*sat
    val hue_p = hue.value * pi_3_inv
    val X = C*(1 - abs(hue_p % 2.0 - 1.0))
    val (r1, g1, b1) = hue_p match {
      case _ if hue_p >= 0 && hue_p < 1 => (C, X, .0)
      case _ if hue_p >= 1 && hue_p < 2 => (X, C, .0)
      case _ if hue_p >= 2 && hue_p < 3 => (.0, C, X)
      case _ if hue_p >= 3 && hue_p < 4 => (.0, X, C)
      case _ if hue_p >= 4 && hue_p < 5 => (X, .0, C)
      case _  => (C, .0, X)
      // case _ if hue_p >= 5 => (C, .0, X)
    }
    val m = l - 0.5 * C
    ColorVector(r1 + m, g1 + m, b1 + m, a)
  }
}

/**
 * Class is helpful in color manipulation. It's components needn't normalization. It can be used for example for
 * creating intermediate colors between the two. It is related to color addition and subtraction by the rules:
 * - subtracting two colors produces color vector
 * - adding to color is performed with color vector.
 *
 * @param r
 * @param g
 * @param b
 * @param a
 */
case class ColorVector(r: Double, g: Double, b: Double, override val a: Double = 1.0) extends AbstractColor {
  import ColorVector._
  import AbstractColor._

  def +(other: ColorVector) = ColorVector(r + other.r, g + other.g, b + other.b, a)
  def -(other: ColorVector) = ColorVector(r - other.r, g - other.g, b - other.b, a)
  def *(factor: Double) = ColorVector(r*factor, g*factor, b*factor, a)
  def /(factor: Double) = ColorVector(r/factor, g/factor, b/factor, a)
  def addR(v: Double = 1.0) = ColorVector(r + v, g, b, a)
  def addG(v: Double = 1.0) = ColorVector(r, g + v, b, a)
  def addB(v: Double = 1.0) = Color(r, g, b + v, a)
  def addH(v: Angle) = hsla(h + v, s, l, a)
  def addS(v: Double) = hsla(h, s + v, l, a)
  def addL(v: Double) = hsla(h, s, l + v, a)
  def setH(new_h: Double) = hsla(new_h, s, l, a)
  def setS(new_s: Double) = hsla(h, new_s, l, a)
  def setL(new_l: Double) = hsla(h, s, new_l, a)
  def setA(new_a: Double) = Color(r, g, b, new_a)

  def toColor(method: Symbol = SAT, softSatFactor: Double = 1.0) = {
    def calcPart(v: Double) = method match {
        case SAT => sat(v)
        case SOFT_SAT => softSat(v*softSatFactor)
        case CYCLE => cycle(v)
        case BOUNCE => bounce(v)
    }
    
    Color(calcPart(r), calcPart(g), calcPart(b), a)
  }

  def toInfColor(method: Symbol = SAT, softSatFactor: Double = .1) = {
    def calcPart(v: Double) = method match {
      case SAT => satBottom(v)
      case SOFT_SAT => softSatBottom(v*softSatFactor)
      case BOUNCE => bounceBottom(v)
    }

    InfiniteColor(calcPart(r), calcPart(g), calcPart(b), a)
  }

}
