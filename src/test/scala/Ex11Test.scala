package aoc

import org.junit.Test
import org.junit.Assert.*

class Ex11Test extends ExTest(Ex11):
  val testcases = Seq(
    TestCase(
       """|...#......
          |.......#..
          |#.........
          |..........
          |......#...
          |.#........
          |.........#
          |..........
          |.......#..
          |#...#.....""", 374L, 82000210L),
    TestCase(exInput, 10231178L, 622120986954L))
