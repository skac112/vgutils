package com.github.skac112.vgutils
import scala.math._

object Bounds {
  def empty: Bounds = Bounds(ori, ori, 'EMPTY_BOUNDS)
  def point(pt: Point): Bounds = Bounds(pt, pt, 'POINT_BOUNDS)
  def inf = Bounds(ori, ori, 'INF_BOUNDS)

  /**
   * Calculates bounds containing sequence of points. Sequence can be of any
   * length including 0 and 1.
   */
  def forPts(pts: Set[Point]) = pts.foldLeft(empty) {(acc: Bounds, pt: Point) =>
    acc + point(pt)
  }
}

/**
 * Class representing rectangular area of drawing together with following special
 * cases:
 * - empty bounds
 * - point
 * - infinite bounds
 */
case class Bounds(tl: Point, br: Point, boundsType: Symbol = 'NORMAL_BOUNDS) {
  import Bounds._
  /**
   * Width of these bounds.
   */
  lazy val w = br.x - tl.x

  /**
   * Height of these bounds.
   */
  lazy val h = br.y - tl.y

  lazy val mid = (tl + br) * .5

  def +(other: Bounds) = (this.boundsType, other.boundsType) match {
    case (_, 'EMPTY_BOUNDS) => this
    case (_, 'INF_BOUNDS) => other
    case ('INF_BOUNDS, _) => this
    case ('EMPTY_BOUNDS, _) => other
    case (_, _) => {
      val tl = minTopLeft(this.tl, other.tl)
      val br = maxBottomRight(this.br, other.br)
      Bounds(tl, br)
    }
  }

  private def minTopLeft(tl1: Point, tl2: Point) = Point(min(tl1.x, tl2.x), min(tl1.y, tl2.y))

  private def maxBottomRight(br1: Point, br2: Point) = Point(max(br1.x, br2.x), max(br1.y, br2.y))

  def move(pt: Point) = Bounds(tl + pt, br + pt)

  /**
   * Zooms in (shrinks bounds, factor > 1) or zooms out (extends bounds, factor < 1).
   */
  def zoom(factor: Double): Bounds = {
    val new_w = w / factor
    val new_h = h / factor
    val p = Point(new_w, new_h) * .5
    Bounds(mid - p, mid + p)
  }

  def area = w * h

  /**
    * Determines if these bounds have some common area with other bounds. It excludes cases of mere touching.
    * @param other
    */
  def intersectsWith(other: Bounds) =
    tl.x < other.br.x &&
    br.x > other.tl.x &&
    tl.y < other.br.y &&
    br.y > other.tl.y

  def isOutsideOf(other: Bounds) =
    tl.x > other.br.x ||
      br.x < other.tl.x ||
      tl.y > other.br.y ||
      br.y < other.tl.y

  def isInside(other: Bounds) =
    tl.x > other.tl.x &&
      br.x < other.br.x &&
      tl.y > other.tl.y &&
      br.y < other.br.y

  /**
   * Determines if a given point lies in or on bounds (not outside).
   */
  def hitTest(pt: Point) = boundsType match {
    case 'NORMAL_BOUNDS => pt.x >= tl.x && pt.y >= tl.y && pt.x <= br.x && pt.y <= br.y
    case 'POINT_BOUNDS => tl == pt
    case 'INF_BOUNDS => true
    case 'EMPTY_BOUNDS => false
  }
}
