package aoc

object Ex12 extends Exercise:
  case class Row(springs: String, choices: Array[Int])
  type Common = Seq[Row]

  def common(input: Iterator[String]) = 
    input.map: line =>
      val Array(springs, c) = line.split(" ")
      val choices = c.split(",").map(_.toInt)
      Row(springs, choices)
    .toSeq

  def count(cs: List[Char], segs: List[Int], curSeg: Int, dotNext: Boolean = false): Int =
    inline def Valid = {
      //println("Valid")
      1
    }
    inline def Invalid = {
      //println("Invalid")
      0
    }
    //println(s"$cs $segs $curSeg")

    def checkHash(cs: List[Char]) = 
      if dotNext then Invalid
      else if curSeg > 1 then
        count(cs, segs, curSeg - 1)
      else if curSeg == 1 then
        count(cs, segs, 0, dotNext = true)
      else
        segs match
          case s :: ss => count('#' :: cs, ss, s)
          case Nil => Invalid

    def checkDot(cs: List[Char]) = 
      if curSeg == 0 then
        count(cs, segs, 0, dotNext = false)
      else
        Invalid 

    cs match
      case '#' :: cs => checkHash(cs)
      case '.' :: cs => checkDot(cs)
      case '?' :: cs => checkHash(cs) + checkDot(cs)
      case Nil => 
        if curSeg == 0 && segs.isEmpty then Valid else Invalid

    
  def part1(rows: Common) =
    rows.map: row => 
      val c = count(row.springs.toList, row.choices.toList, 0)
      //println(s"${row.springs} ${row.choices.toList} $c")
      c
    .sum

  def part2(rows: Common) = 
    rows.map: row => 
      val row5 = (1 to 5).map:
        _ => row.springs
      .reduce((a, b) => a ++ "?" ++ b)
      val c5 = (1 to 5).flatMap:
        _ => row.choices
      println(s"${row5.mkString("")} ${c5.toList}")
      val c = count(row5.toList, c5.toList, 0)
      println(s"  count = $c")
      c.toLong
    .sum