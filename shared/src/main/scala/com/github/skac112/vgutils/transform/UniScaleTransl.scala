package com.github.skac112.vgutils.transform
import com.github.skac112.vgutils.Point

/**
  * Scale and translation with uniform scale.
  * @param scale
  * @param translation
  */
case class UniScaleTransl(scale: Double, translation: Point) extends Affine(scale, .0, .0, scale,
  translation.x, translation.y) {
  /**
    * Zooms (in or out) about specific point - returns new transformation.
    * Image of this specific point after new transformation remains the same as after the old one.
    * @param pt
    * @param zoom
    */
  def zoomAt(pt: Point, zoom: Double): UniScaleTransl =
    UniScaleTransl(scale*zoom, translation + pt*scale*(1 - zoom))
}
