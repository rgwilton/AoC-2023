package aoc

import org.junit.Test
import org.junit.Assert.*

class Ex12Test extends ExTest(Ex12):
  val testcases = Seq(
    TestCase(
       """|???.### 1,1,3
          |.??..??...?##. 1,1,3
          |?#?#?#?#?#?#?#? 1,3,1,6
          |????.#...#... 4,1,1
          |????.######..#####. 1,6,5
          |?###???????? 3,2,1""", 21L, 525152L),
    TestCase(exInput, 7633L, 23903579139437L))

