package aoc

import org.junit.Test
import org.junit.Assert.*

class Ex06Test extends ExTest(Ex06):
  val testcases = Seq(
    TestCase(
       """|Time:      7  15   30
          |Distance:  9  40  200""", 288, 71503L),
    TestCase(exInput, 2449062, 33149631L)
  )