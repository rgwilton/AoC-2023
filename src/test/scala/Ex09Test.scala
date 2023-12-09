package aoc

import org.junit.Test
import org.junit.Assert.*

class Ex09Test extends ExTest(Ex09):
  val testcases = Seq(
    TestCase(
      """|0 3 6 9 12 15
         |1 3 6 10 15 21
         |10 13 16 21 30 45""", 114, 2),
    TestCase(exInput, 1901217887, 905))
