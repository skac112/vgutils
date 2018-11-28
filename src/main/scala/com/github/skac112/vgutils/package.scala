package com.github.skac112

/**
 * @author slawek
 */
import vgutils.Point
import vgutils.transform._

package object vgutils {
  type Points = Seq[Point]
  type PtSet = Set[Point]
  implicit def Tuple2Point(Tuple: Tuple2[Double, Double]) = Point(Tuple._1, Tuple._2)
  implicit def double2Point(value: Double) = Point(value, .0)
  // implicit def angle2Double(angle: Angle) = angle.normVal
  implicit def angle2Double(angle: Angle) = angle.value
  implicit def double2Angle(value: Double) = Angle(value)
  def ori = Point(0.0, 0.0)
  implicit def doubleToRot(value: Double) = Rotation(value)
  implicit def angleToRot(angle: Angle) = Rotation(angle.value)
  implicit def stringToCol(webStr: String) = new Color(webStr)
  implicit def tripleToCol(triple: (Double, Double, Double)) = Color(triple._1, triple._2, triple._3)
  implicit def quadToCol(quad: (Double, Double, Double, Double)) = Color(quad._1, quad._2, quad._3, quad._4)
}
