package aoc

object Ex09 extends Exercise:
  type Common = Seq[(Int, Int)]

  // Recursively calculate the first and last values
  def calcNext(ints: IndexedSeq[Int]): (Int, Int) =
    if ints.forall(_ == 0) then (0, 0)
    else
      val diffs =
        ints.sliding(2).map:
          case Seq(a, b) => b - a
        .toIndexedSeq
      val (fi, li) = calcNext(diffs)
      (ints.head - fi, ints.last + li)

  def common(input: Iterator[String]) = 
    input.map: line =>
      line.split(" ").map(_.toInt).toIndexedSeq
    .toSeq
    .map(calcNext)

  def part1(answers: Common) = answers.map(_._2).sum
  def part2(answers: Common) = answers.map(_._1).sum