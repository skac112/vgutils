package com.github.skac112.vgutils
import scala.math._

object Color {
  def red(v: Double = 1.0) = new Color(v, .0, .0)
  def green(v: Double = 1.0) = new Color(.0, v, .0)
  def blue(v: Double = 1.0) = new Color(.0, .0, v)
  def cyan(v: Double = 1.0) = new Color(.0, v, v)
  def magenta(v: Double = 1.0) = new Color(v, .0, v)
  def yellow(v: Double = 1.0) = new Color(v, v, .0)
  lazy val white = Color(1.0, 1.0, 1.0)
  lazy val black = Color(.0, .0, .0)
  val transparent = Color(.0, .0, .0, .0)
  def hex2int(hex: String): Int = Integer.parseInt(hex, 16)
  def hex2double(hex: String) = hex2int(hex) / 255.0
}

import AbstractColor._

final case class Color(startR: Double, startG: Double, startB: Double, startA: Double = 1.0) extends AbstractColor {
  override lazy val r = sat(startR)
  override lazy val g = sat(startG)
  override lazy val b = sat(startB)
  override lazy val a = sat(startA)

  def canEqual(a: Any) = a.isInstanceOf[Color]

  override def equals(other: Any): Boolean = other match {
    case other: Color => other.canEqual(this) && this.r == other.r && this.g == other.g &&
      this.b == other.b && this.a == other.a
    case _ => false
  }

  override def hashCode: Int = (rInt + 1)*(gInt + 1)*(bInt + 1)

  def this(webStr: String) = this(hex2double(webStr.substring(0, 2)),
    hex2double(webStr.substring(2, 4)),
    hex2double(webStr.substring(4, 6)))

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
}
