package com.github.skac112.vgutils

import scala.math._

object AbstractColor {
  def hex2int(hex: String): Int = Integer.parseInt(hex)
  def hex2double(hex: String) = hex2int(hex) / 255.0

  lazy val SAT = Symbol("SAT")
  lazy val SOFT_SAT = Symbol("SOFT_SAT")
  lazy val CYCLE = Symbol("CYCLE")
  lazy val BOUNCE = Symbol("BOUNCE")

  /**
   * Saturation function clipping all values above 1 and below 0.
   * @param v
   * @return
   */
  def sat(v: Double) = v match {
    case v if v > 1.0 => 1.0
    case v if v < 0.0 => 0.0
    case v => v
  }

  /**
   * Saturartion function applicable to parts of InfiniteColor. Only values below 0 are saturated (clipped to 0).
   * @param v
   */
  def satBottom(v: Double) = if (v >= 0.0) v else 0.0

  def softSat(v: Double) = .5*tanh(2.0*v - 1.0)+.5

  def softSatBottom(v: Double, f: Double = 1.0): Double = .5/f*(f*v + sqrt(f*f*v*v+4))

  def cycle(v: Double) = v - floor(v)

  def bounce(v: Double) = {
    val v2 = v/2.0
    // range <0; 2>
    2*(v2 - floor(v2)) match {
      case v if v < 1.0 => v
      case v => 2.0 - v
    }
  }

  def bounceBottom(v: Double) = if (v >= 0.0) v else -v
}

trait AbstractColor {
  def r: Double
  def g: Double
  def b: Double
  def a: Double = 1.0

  override lazy val toString = a match {
    case 1.0 => {
      // konwersja wartosci Double w zakresie <0; 1> na dwucyfrowy lancuch heksadecymalny
      val to_hex = (value: Double) => f"${round(value * 255)}%02x"
      "#" + (Seq(r, g, b) map to_hex reduceLeft {
        _ + _
      })
    }

    case _ => s"rgba($rInt, $gInt, $bInt, $a)"
  }

  lazy val rInt: Int = round(r * 255).toInt

  lazy val gInt: Int = round(g * 255).toInt

  lazy val bInt: Int = round(b * 255).toInt

  lazy val aInt: Int = round(a * 255).toInt

  lazy val toInt: Int = (aInt << 24) + (rInt << 16) + (gInt << 8) + bInt

  private lazy val M =  List(r, g, b).max

  private lazy val m = List(r, g, b).min

  /**
   * Chroma
   */
  private lazy val C = M - m

  /**
   * Wartosc hue w modelu HSL. Zrodlo:
   * https://en.wikipedia.org/wiki/HSL_and_HSV
   */
  lazy val h: Angle = {
    val hue_60 = (r, g, b, C) match {
      case (_, _, _, .0) => .0
      case _ if M == r => (g - b) / C % 6
      case _ if M == g => (b - r) / C + 2
      case _ => (r - g) / C + 4
    }
    Angle(hue_60 * Pi / 3.0)
  }

  /**
   * Wartosc lightness (L) w modelu HSL. Zakres: <0; 1>. Zrodlo:
   * https://en.wikipedia.org/wiki/HSL_and_HSV
   */
  lazy val l = 0.5 * (M + m)

  /**
   * Nasycenie (S) w modelu HSL. Zakres: <0; 1>. Zrodlo:
   * https://en.wikipedia.org/wiki/HSL_and_HSV
   */
   lazy val s: Double = l match {
     case 1.0 => 0
     case 0 => 1.0
     case _ => C / (1.0 - abs(2*l - 1.0))
   }

  def toColorVector = ColorVector(r, g, b, a)
  def +(other: Color): ColorVector = toColorVector + other.toColorVector
  def -(other: Color): ColorVector = toColorVector - other.toColorVector
}
