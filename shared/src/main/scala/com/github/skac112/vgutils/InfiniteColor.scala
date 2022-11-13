package com.github.skac112.vgutils

final case class InfiniteColor(startR: Double, startG: Double, startB: Double, startA: Double = 1.0)
  extends AbstractColor {
    import AbstractColor._
    override lazy val r = satBottom(startR)
    override lazy val g = satBottom(startG)
    override lazy val b = satBottom(startB)
    override lazy val a = satBottom(startA)
}
