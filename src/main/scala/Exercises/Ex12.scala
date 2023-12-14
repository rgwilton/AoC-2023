package aoc

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._

object Ex12 extends Exercise:
  enum Segment:
    case Hash(count: Int)
    case Dot(minCount: Int)
  import Segment._

  case class Row(springs: String, choices: IndexedSeq[Int]):
    val requiredHashes = choices.sum
    val knownHashes = springs.count(_ == '#')
    val knownDots = springs.count(_ == '.')
    val maxUnknownDots = springs.length - knownHashes - knownDots
    val maxUnknownHashes = requiredHashes - knownHashes
    
    val segments = 
      List(Dot(0)) ++
      choices.flatMap: c =>
        Seq(Dot(1), Hash(c))
      .tail ++
      Seq(Dot(0))

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
      val c = count3(row)
      // println(s"${row.springs} ${row.choices.toList} $c")
      c
    .sum

  def count2(cs: List[Char], segs: List[Int], curSeg: Int, maxUnknownDots: Int, dotNext: Boolean = false): Int =
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
        count2(cs, segs, curSeg - 1, maxUnknownDots)
      else if curSeg == 1 then
        count2(cs, segs, 0, maxUnknownDots, dotNext = true)
      else
        segs match
          case s :: ss => 
            if s > 1 then
              count2(cs, ss, s - 1, maxUnknownDots)
            else
              count2(cs, ss, 0, maxUnknownDots, dotNext = true)
          case Nil => Invalid

    def checkDot(cs: List[Char], unknown: Boolean = false) = 
      if unknown then 
        if maxUnknownDots == 0 then Invalid
        else if curSeg == 0 then
          count2(cs, segs, 0, maxUnknownDots - 1)
        else Invalid
      else
        if curSeg == 0 then
          count2(cs, segs, 0, maxUnknownDots)
        else
          Invalid 

    cs match
      case '#' :: cs => checkHash(cs)
      case '.' :: cs => checkDot(cs)
      case '?' :: cs => 
        checkHash(cs) + checkDot(cs, unknown = true)
      case Nil => 
        if curSeg == 0 && segs.isEmpty then Valid else Invalid


  def count2b(cs: List[Char], segs: List[Int], maxUnknownDots: Int, dotNext: Boolean = false): Int =
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
      else if segs.head > 1 then
        count2b(cs, segs.head - 1 :: segs.tail, maxUnknownDots)
      else if segs.head == 1 then
        count2b(cs, 0 :: segs.tail, maxUnknownDots, dotNext = true)
      else
        segs match
          case s :: ss => 
            if s > 1 then
              count2b(cs, s - 1 :: ss, maxUnknownDots)
            else
              count2b(cs, 0 :: ss, maxUnknownDots, dotNext = true)
          case Nil => Invalid

    def checkDot(cs: List[Char], unknown: Boolean = false) = 
      if unknown then 
        if maxUnknownDots == 0 then Invalid
        else if segs.head == 0 then
          count2b(cs, segs, maxUnknownDots - 1)
        else Invalid
      else
        if segs.head == 0 then
          count2b(cs, segs, maxUnknownDots)
        else
          Invalid 

    cs match
      case '#' :: cs => checkHash(cs)
      case '.' :: cs => checkDot(cs)
      case '?' :: cs => 
        checkHash(cs) + checkDot(cs, unknown = true)
      case Nil => 
        if segs.isEmpty then Valid else Invalid



  case class Result(chars: List[Char], segs: List[Segment], count: Int)
  def count3(r: Row, resultCache: scala.collection.mutable.Map[Result, Long] = scala.collection.mutable.Map.empty): Long =
    inline def Valid = {
      //println("Valid")
      1L
    }
    inline def Invalid = {
      //println("Invalid")
      0L
    }
    //println(s"${r.springs} ${r.segments}")
    def check(chars: List[Char], matched: List[Char], segs: List[Segment], maxUDots: Int, maxUHashes: Int, count: Int): Long =
      //println(s"$chars, $segs, $count, $maxUDots, $maxUHashes")
      val v = 
        if count == 0 then
          resultCache.get(Result(chars, segs, count))
        else None
      v match
        case Some(value) => value
        case None =>
      
          val res = 
            if chars.isEmpty then
              if segs.length == 1 then 
                //println(s"Matched ${matched.reverse.mkString}, $segs, $count, $maxUDots, $maxUHashes")
                Valid
              else Invalid
            else
              val curSeg = segs.head
              curSeg match
                case Hash(limit) =>
                  chars match
                    case c :: cs =>
                      c match
                        case '#' if count < limit => 
                          if count + 1 == limit then
                            check(cs, '#' :: matched, segs.tail, maxUDots, maxUHashes, 0)
                          else
                            check(cs, '#' :: matched, segs, maxUDots, maxUHashes, count + 1)
                        case '.' if count == limit =>
                          check(chars, matched, segs.tail, maxUDots, maxUHashes, 0)
                        case '?' if maxUHashes > 0 && count < limit => 
                          if count + 1 == limit then
                            check(cs, '#' :: matched, segs.tail, maxUDots, maxUHashes - 1, 0)
                          else
                            check(cs, '#' :: matched, segs, maxUDots, maxUHashes - 1, count + 1)
                        case '?' if maxUDots > 0 && count == limit =>
                          check(chars, matched, segs.tail, maxUDots - 1, maxUHashes, 0)
                        case _ => Invalid

                case Dot(min) =>
                  chars match
                    case c :: cs =>
                      c match
                        case '.' =>
                          check(cs, '.' :: matched, segs, maxUDots, maxUHashes, count + 1)
                        case '#' if count >= min && segs.tail.nonEmpty => 
                          check(chars, matched, segs.tail, maxUDots, maxUHashes, 0)
                        case '?' =>
                          (if maxUDots > 0 then
                            check(cs, '.' :: matched, segs, maxUDots - 1, maxUHashes, count + 1)
                          else Invalid) +
                          (if count >= min && segs.tail.nonEmpty then
                            check(chars, matched, segs.tail, maxUDots, maxUHashes, 0)
                          else Invalid)
                        case _ => Invalid
          if count == 0 then
            resultCache += Result(chars, segs, count) -> res
          res

    val result = check(r.springs.toList, List(), r.segments, r.maxUnknownDots, r.maxUnknownHashes, 0)
    //println(resultCache.size)
    result


  def part2(rows: Common) =
    val resultCache = scala.collection.mutable.Map[Result, Long]()

    rows.toIndexedSeq.par.map: row => 
      val springs5 = (1 to 5).map:
        _ => row.springs
      .reduce((a, b) => a ++ "?" ++ b)
      val c5 = (1 to 5).flatMap:
        _ => row.choices
      val row5 = Row(springs5, c5)
      // println(s"$row5.springs ${row5.segments}, max dots = ${row5.maxUnknownDots}, max hashes = ${row5.maxUnknownHashes}")
      val c = count3(row5)
      // println(s"  count = $c")
      c.toLong
    .sum