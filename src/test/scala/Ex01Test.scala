package aoc

import org.junit.Test
import org.junit.Assert.*

class Ex01Test extends ExTest(Ex01):  
  val testcases = Seq(
    TestCase(
    """|two1nine
       |eightwothree
       |abcone2threexyz
       |xtwone3four
       |4nineeightseven2
       |zoneight234
       |7pqrstsixteen""", 209, 281),
    TestCase(exInput, 54953, 53868))

