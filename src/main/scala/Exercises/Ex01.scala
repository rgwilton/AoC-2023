package aoc

object Ex01 extends Exercise:
  override type Common = Iterable[String]

  def common(input: Iterator[String]) = input.toSeq

  def part1(common: Common) = 
    common.map:
      line =>
          (line.find(_.isDigit).getOrElse(0).toString ++
          line.findLast(_.isDigit).getOrElse(0).toString)
          .toInt
    .sum

  def part2(common: Common) = 
    val numWords =
      IndexedSeq("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

    val res = common.map:
      line =>
        def getFirst(range: Range) =
          range.iterator.map:
            offset => 
              if line(offset).isDigit then line(offset) - '0'
              else
                (1 to 9).find:
                  i => line.startsWith(numWords(i), offset)
                .getOrElse(-1)                            
          .filter(_ > 0)
          .next

        val first = getFirst(0 until line.length)
        val last = getFirst(line.length - 1 to 0 by -1)
        first * 10 + last
        
    res.sum
