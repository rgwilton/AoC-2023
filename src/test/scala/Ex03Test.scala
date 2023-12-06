package aoc

import org.junit.Test
import org.junit.Assert.*

class Ex03Test extends ExTest(Ex03):
  val testcases = Seq(
    TestCase(
     """|467..114..
        |...*......
        |..35..633.
        |......#...
        |617*......
        |.....+.58.
        |..592.....
        |......755.
        |...$.*....
        |.664.598..""", 4361, 467835),
    TestCase(exInput, 498559, 72246648)
  )
  