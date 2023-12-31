package aoc

import org.junit.Test
import org.junit.Assert.*

class Ex10Test extends ExTest(Ex10):
  val testcases = Seq(
    TestCase(
       """|.....
          |.S-7.
          |.|.|.
          |.L-J.
          |.....""", 4, 1),
    TestCase(
       """|...........
          |.S-------7.
          |.|F-----7|.
          |.||.....||.
          |.||.....||.
          |.|L-7.F-J|.
          |.|..|.|..|.
          |.L--J.L--J.
          |...........""", 23, 4),      
    TestCase(
       """|.F----7F7F7F7F-7....
          |.|F--7||||||||FJ....
          |.||.FJ||||||||L7....
          |FJL7L7LJLJ||LJ.L-7..
          |L--J.L7...LJS7F-7L7.
          |....F-J..F7FJ|L7L7L7
          |....L7.F7||L7|.L7L7|
          |.....|FJLJ|FJ|F7|.LJ
          |....FJL-7.||.||||...
          |....L---J.LJ.LJLJ...""", 70, 8),
    TestCase(exInput, 7066, 401))