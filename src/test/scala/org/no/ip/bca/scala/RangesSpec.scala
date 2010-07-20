package org.no.ip.bca.scala

import org.specs._

object RangesSpec extends RangesTest
class RangesTest extends SpecificationWithJUnit {
  implicit def intTupleToLongPair(t: (Int, Int)): Ranges.Pair = Ranges.Pair(t._1.toLong, t._2.toLong)

  "unions" should {
    "single point" >> {
      val ranges = Ranges(2)
      "distinct point before" >> { ranges | 0 mustEqual Ranges((0, 1), (2, 3)) }
      "distinct point after" >> { ranges | 4 mustEqual Ranges((2, 3), (4, 5)) }
      "adjacent point before" >> { ranges | 1 mustEqual Ranges(1, 3) }
      "adjacent point after" >> { ranges | 3 mustEqual Ranges(2, 4) }
      "same point" >> { ranges | 2 mustEqual Ranges(2, 3) }
    }
    "two disjoint points" >> {
      val ranges = Ranges((2, 3), (4, 5))
      "point between" >> { ranges | 3 mustEqual Ranges(2, 5) }
      "point between2" >> { Ranges(3) | ranges mustEqual Ranges(2, 5) }
      "point before" >> { ranges | 1 mustEqual Ranges((1, 3), (4, 5)) }
      "point before2" >> { Ranges(1) | ranges  mustEqual Ranges((1, 3), (4, 5)) }
      "point after" >> { ranges | 5 mustEqual Ranges((2, 3), (4, 6)) }
      "point after2" >> { Ranges(5) | ranges mustEqual Ranges((2, 3), (4, 6)) }
    }
    "overlapping ranges" >> {
      val ranges = Ranges(2, 5)
      "bigger on both sides" >> { ranges | (1, 6) mustEqual Ranges(1, 6) }
      "smaller" >> { ranges | (3, 4) mustEqual Ranges(2, 5) }
      "left overlap" >> { ranges | (1, 3) mustEqual Ranges(1, 5) }
      "right overlap" >> { ranges | (4, 6) mustEqual Ranges(2, 6) }
    }
    "zig-zag" >> { Ranges((0, 2), (5, 7), (10, 12)) | Ranges((2, 5), (7, 10)) mustEqual Ranges(0, 12) }
  }

  "intersections" should {
    val ranges = Ranges(2, 5)
    "bigger on both sides" >> { ranges & (1, 6) mustEqual Ranges(2, 5) }
    "smaller" >> { ranges & (3, 4) mustEqual Ranges(3, 4) }
    "left overlap" >> { ranges & (1, 3) mustEqual Ranges(2, 3) }
    "right overlap" >> { ranges & (4, 6) mustEqual Ranges(4, 5) }
    "disjoint" >> { ranges & (8, 10) mustEqual Ranges() }
    "straddle" >> { ranges & Ranges((1, 3), (4, 7)) mustEqual Ranges((2, 3), (4, 5)) }
    "straddle2" >> { Ranges((1, 3), (4, 7)) & ranges mustEqual Ranges((2, 3), (4, 5)) }
    "zig-zag" >> { Ranges((0, 2), (5, 8), (11, 14)) & Ranges((1, 6), (7, 12)) mustEqual Ranges((1, 2), (5, 6), (7, 8), (11, 12)) }
    "adjacent" >> { Ranges((0, 3), (6, 10)) & Ranges(3, 6) mustEqual Ranges.empty }
    "adjacent2" >> { Ranges(3, 6) & Ranges((0, 3), (6, 10)) mustEqual Ranges.empty }
  }

  "complement" should {
    val ranges = Ranges(0, 10)
    "a" >> { ranges / Ranges() mustEqual Ranges(0, 10) }
    "b" >> { ranges / Ranges(2, 5) mustEqual Ranges((0, 2), (5, 10)) }
    "c" >> { Ranges((0, 4), (5, 6), (8, 10)) / Ranges(2, 9) mustEqual Ranges((0, 2), (9, 10)) }
    "d" >> { Ranges((0, 4), (5, 6), (8, 10)) / Ranges((-3, -2), (-1, 0)) mustEqual Ranges((0, 4), (5, 6), (8, 10)) }
  }
}