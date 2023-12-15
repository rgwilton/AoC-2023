package aoc

import org.junit.Test
import org.junit.Assert.*

class Ex15Test extends ExTest(Ex15):
  val testcases = Seq(
    TestCase(
      """rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7""", 1320, 145),
    TestCase(exInput, 505459, 228508))

