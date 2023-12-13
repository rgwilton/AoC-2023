package aoc

import org.junit.Test
import org.junit.Assert.*

class Ex13Test extends ExTest(Ex13):
  val testcases = Seq(
    TestCase(
       """|#.##..##.
          |..#.##.#.
          |##......#
          |##......#
          |..#.##.#.
          |..##..##.
          |#.#.##.#.
          |
          |#...##..#
          |#....#..#
          |..##..###
          |#####.##.
          |#####.##.
          |..##..###
          |#....#..#""", 405, 400),
    TestCase(exInput, 33122, 32312))

