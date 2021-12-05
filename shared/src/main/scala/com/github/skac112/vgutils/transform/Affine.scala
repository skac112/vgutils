package com.github.skac112.vgutils.transform

import com.github.skac112.vgutils._
import com.github.skac112.vgutils.transform.linear.Linear

object Affine {
  /**
   * Mirror transformation due to given straight line (parameterized by a, b, c)
   */
  def mirror(a: Double, b: Double, c: Double) = {
    val den_inv = 1 / (a*a + b*b)
    val bc_trans = -2*a*b*den_inv
    new Affine(2*b*b*den_inv - 1, bc_trans, bc_trans, 2*a*a*den_inv - 1, -2*c*a*den_inv, -2*c*b*den_inv)
  }
}

/**
 * @author slawek
 */
class Affine(val a: Double, val b: Double, val c: Double, val d: Double, val e: Double, val f: Double) {
  def transPt(Pt: Point) = Point(a*Pt.x + b*Pt.y + e, c*Pt.x + d*Pt.y + f)

//  /**
//   * Zwraca wyznacznik liniowej czesci przeksztalcenia. Wartosc wyznacznika okresla, jak zmienia sie powierzchnia
//   * figury poddanej przeksztalceniu. Wartosc wieksza od 1 oznacza powiekszanie powierzchni, mniejsza niz 1 - zmniejszanie.
//   */
//  def linDet = a*c - b*d

  /**
   * Zwraca liniowa czesc przeksztalcenia
   */
  def lin = new Linear(a, b, c, d)

  /**
   * Zwraca przeksztalcenie translacji wystepujacej w tym przeksztalceniu
   */
  def transl = Transl(e, f)

  def translPt = Point(e, f)

  /**
   * Transformacja odwrotna.
   */
  lazy val inv: Affine = {
    val lin_inv = lin.inv
    lin_inv - (lin_inv.apply(translPt))
  }
}

//class Scale(ScaleX: Double, ScaleY: Double) extends Affine(ScaleX, 0.0, 0.0, ScaleY, 0.0, 0.0)
//case class UniScale(Scale: Double) extends Scale(Scale, Scale)
//case class Move(MoveX: Double, MoveY: Double) extends Affine(1.0, 0.0, 0.0, 1.0, MoveX, MoveY)
//case class RotTrans(Rot: Double) extends Affine(cos(Rot), -sin(Rot), sin(Rot), cos(Rot), 0.0, 0.0)
//class Linear(A: Double, B: Double, C: Double, D: Double) extends Affine(A, B, C, D, 0.0, 0.0)
