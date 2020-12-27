package com.github.skac112.vgutils

object RichDouble {
  val EPS: Double = 1e-13
}

case class RichDouble(value: Double) {
  import RichDouble._

  def about(other: Double): Boolean = {
    val diff = value - other
    diff >= 0 && diff <= EPS || diff <= 0 && diff > -EPS
  }

  def about(other: RichDouble): Boolean = about(other.value)
}
