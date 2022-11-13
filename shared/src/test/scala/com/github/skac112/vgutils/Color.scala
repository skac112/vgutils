package com.github.skac112.vgutils

import utest._

object HelloTests extends TestSuite{
  val tests = Tests{
    test("Colors can be created by specifying parts with possible overflow/underflow"){
      val overflow_color = Color(1.4, -3, .5)
      val color = Color(1.0, 0, .5)
      assert(overflow_color == color)
    }

    test("Colors can be added with overflow protection"){
      val color1 = Color(.5, .5, .5)
      val color2 = Color(.2, .6, .2)
      val color_add: Color = color1 + color2
      assert(color_add == Color(.7, 1.0, .7))
    }

    test("Colors can be subtracted with underflow protection"){
      val color1 = Color(.5, .5, .5)
      val color2 = Color(.2, .6, .2)
      val color_sub: Color = color1 - color2
      assert(color_sub == Color(.3, 0, .3))
    }
  }
}
