package com.github.skac112.vgutils
import scala.math._

object Angle {
  /**
   * Returns value normalized to <0; 2*Pi>.
   */
  def normVal(value: Double): Double = value match {
    case value if value < 0.0 => value + 2 * Pi * Math.ceil(0.5 * abs(value) / Pi)
    case value if value > 2 * Pi => value - 2 * Pi * Math.floor(0.5 * value / Pi)
    case _ => value
  }

  lazy val dtrF = Pi / 180.0

  lazy val rtdF = 180.0 / Pi

  def fromDeg(deg: Double) = new Angle(dtrF * deg)
//   def fromVal(value: Double) = new Angle(value)
//   def fromDir(x: Double, y: Double) = new Angle(x, y)
  def apply(value: Double) = new Angle(value)
  def apply(x: Double, y: Double) = new Angle(x, y)
}

/**
 * Angle with value in range <0; 2*Pi). It is not appropriate to represent angle
 * increases, because they can have arbitrary value - positive or negative.
 * @author slawek
 */
case class Angle(initX: Double, initY: Double, initValue: Double) {
  import Angle._

  lazy val value: Double = initValue match {
    case 0 => (initX, initY) match {
      case (0, 0) => 0
      case (0, y) => if (y > 0) .5*Pi else 1.5*Pi
      case (x, 0) => if (x > 0) 0 else Pi
      case (x, y) => normVal(atan2(y, x))
    }
    case v => normVal(v)
  }

  lazy val x = (initX, initY) match {
    case (0, 0) => cos(value)
    case (x, _) => x / modulus
  }

    lazy val y = (initX, initY) match {
    case (0, 0) => sin(value)
    case (_, y) => y / modulus
  }

  lazy val modulus = sqrt(initX*initX + initY*initY)

  lazy val complement = Angle(0, 0, 2*Pi - value)

  lazy val unary_- = complement

  lazy val opposite = Angle(.0, .0, value + Pi)

  lazy val versor = Point(x, y)

  /**
   * Creates angle from vector (internally transforming it to a versor)
   */
  def this(initX: Double, initY: Double) = this(initX, initY, 0)

  /**
   * Creates angle from explicit value of angle (normalizing it if necessary).
   */
  def this(initValue: Double) = this(0, 0, initValue)

  def +(other: Angle) = Angle(0, 0, value + other.value)
  def +(other: Double) = Angle(0, 0, value + other)
  def -(other: Angle) = Angle(0, 0, value - other.value)
  def -(other: Double) = Angle(0, 0, value - other)
  def *(factor: Double) = Angle(0, 0, value * factor)
  def /(factor: Double) = Angle(0, 0, value / factor)
  // def normalize(value: Double): Angle = Angle(Angle.normVal(value))
  // lazy val normVal: Double = Angle.normVal(value)
  def toDeg = rtdF * value
  // def opposite = Angle(0, 0, value + Pi)

  /**
   * Determines if this angle lies in a given range. Range spans from start to
   * end in positive direction.
   */
  def between(start: Angle, end: Angle): Boolean = {
    val sn = start.value
    val en = end.value
    (sn <= en && value >= sn && value <= en) || (sn >= en && (value >= sn || value <= en))
  }

  def map(mapF: (Double) => Double) = Angle(0, 0, mapF(value))
  def flatMap(fmapF: (Double) => Angle): Angle = fmapF(value)

  lazy val isAcute = value < .5*Pi
  lazy val isRight = value == .5*Pi
  lazy val isObtuse = value > .5*Pi && value < Pi
  lazy val isStraight = value == Pi
  lazy val isReflex = value > 1.5*Pi
  lazy val isFull = value == 2*Pi
}
