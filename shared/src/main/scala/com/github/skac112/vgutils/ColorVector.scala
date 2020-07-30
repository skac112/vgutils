package com.github.skac112.vgutils

/**
 * Class is helpful in color manipulation. It's components needn't normalization. It can be used for example for
 * creating intermediate colors between the two. It is related to color addition and subtraction by the rules:
 * - subtracting two colors produces color vector
 * - adding to color is performed with color vector.
 *
 * @param r
 * @param g
 * @param b
 * @param a
 */
case class ColorVector(r: Double, g: Double, b: Double, a: Double = 1.0) {
  def *(factor: Double) = ColorVector(r*factor, g*factor, b*factor, a)
  def /(factor: Double) = ColorVector(r/factor, g/factor, b/factor, a)
}
