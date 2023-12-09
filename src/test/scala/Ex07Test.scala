package aoc

import org.junit.Test
import org.junit.Assert.*

class Ex07Test extends ExTest(Ex07):
  val testcases = Seq(
    TestCase(
     """|32T3K 765
        |T55J5 684
        |KK677 28
        |KTJJT 220
        |QQQJA 483""", 6440, 5905),
    TestCase(exInput, 253910319, 254083736)
  )