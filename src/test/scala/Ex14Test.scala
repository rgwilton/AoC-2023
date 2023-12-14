package aoc

import org.junit.Test
import org.junit.Assert.*

class Ex14Test extends ExTest(Ex14):
  val testcases = Seq(
    TestCase(
       """|O....#....
          |O.OO#....#
          |.....##...
          |OO.#O....O
          |.O.....O#.
          |O.#..O.#.#
          |..O..#O..O
          |.......O..
          |#....###..
          |#OO..#....""", 136, 64),
    TestCase(exInput, 108759, 89089))

