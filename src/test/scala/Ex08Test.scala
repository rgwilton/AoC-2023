package aoc

import org.junit.Test
import org.junit.Assert.*

class Ex08Test extends ExTest(Ex08):
  val testcases = Seq(
    TestCase(
       """|RL
          |
          |AAA = (BBB, CCC)
          |BBB = (DDD, EEE)
          |CCC = (ZZZ, GGG)
          |DDD = (DDD, DDD)
          |EEE = (EEE, EEE)
          |GGG = (GGG, GGG)
          |ZZZ = (ZZZ, ZZZ)""", 0, 2),
    TestCase(
       """|LLR
          |
          |AAA = (BBB, BBB)
          |BBB = (AAA, ZZZ)
          |ZZZ = (ZZZ, ZZZ)""", 0, 6),
    TestCase(
       """|LR
          |
          |11A = (11B, XXX)
          |11B = (XXX, 11Z)
          |11Z = (11B, XXX)
          |22A = (22B, XXX)
          |22B = (22C, 22C)
          |22C = (22Z, 22Z)
          |22Z = (22B, 22B)
          |XXX = (XXX, XXX)""", 0, 6),
    TestCase(exInput, 0, 0)
  )
