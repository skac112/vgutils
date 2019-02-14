package com.github.skac112.vgutils

/**
 * Straight line parameterized by equation: ax + by + c = 0.
 */
case class Line(a: Double, b: Double, c: Double) {
  def isParallelTo(other: Line) = a*other.b - b*other.a != 0    
}
