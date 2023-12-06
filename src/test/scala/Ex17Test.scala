package aoc

import org.junit.Test
import org.junit.Assert.*

class Ex17Test extends ExTest(Ex17):
  val testcases = Seq(
    TestCase(
   ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>", 3068, 0),
    TestCase(exInput, 3161, 0))
