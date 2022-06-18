package com.github.skac112.vgutils.transform

import com.github.skac112.vgutils._
import com.github.skac112.vgutils.transform.linear._

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
class Affine(val a: Double, val b: Double, val c: Double, val d: Double, val e: Double, val f: Double) extends PtTrans {
  def apply(pt: Point) = Point(a*pt.x + b*pt.y + e, c*pt.x + d*pt.y + f)

  /**
   * Zwraca liniowa czesc przeksztalcenia
   */
  def lin = Linear(a, b, c, d)

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