package aoc

import fastparse._, MultiLineWhitespace._

object Ex05 extends Exercise:
  type Common = Almanac

  case class MRange(dest: Long, src: Long, range: Long)
  case class MMap(from: String, to:String, ranges: Seq[MRange]):
    val sortedRanges = ranges.sortBy(_.src).toList
  case class Almanac(seeds: Seq[Long], maps: Seq[MMap]):
    val mapsMap = 
      maps.map(mmap => mmap.from -> mmap).toMap

  object Parser:
    def num[$: P] = P( CharsWhileIn("0-9").!.map(_.toLong))
    def lowerchars[$: P] = P( CharsWhileIn("a-z").!.map(_.toString))
    def seeds[$: P] = P("seeds:" ~/ num.rep)
    def mapRange[$: P] = P(num ~/ num ~/ num).map(MRange.apply)
    def map[$: P] = P(lowerchars ~/ "-to-" ~/ lowerchars ~/ "map:" ~/ mapRange.rep(1)).map(MMap.apply)
    def almanac[$: P] = P(seeds ~/ map.rep ~/ End).map(Almanac.apply)

  def common(input: Iterator[String]) = 
    parse(input.mkString("\n"), Parser.almanac(_), true).get.value

  def findIndex(almanac: Almanac, seed: Long) =
    var category = "seed"
    var index = seed
    while category != "location" do
      val mmap = almanac.mapsMap(category)
      category = mmap.to
      index = 
        mmap.ranges.find:
          mr => index >= mr.src && index < mr.src + mr.range
        match
          case Some(MRange(dest, src, range)) =>
            index - src + dest
          case None =>
            index
    index

  def part1(almanac: Almanac) = 
    almanac.seeds.map:
      seed => findIndex(almanac, seed)
    .min

  def part2(almanac: Almanac) =
    def check(low: Long, high: Long): Long =
      val l = findIndex(almanac, low)
      val h = findIndex(almanac, high)
      if h - l != high - low then
        val midpoint = (high + low) / 2
        if (midpoint > low && midpoint < high) then
          check(low, midpoint) min
          check(midpoint, high)
        else
          l min h
      else
        l

    almanac.seeds.grouped(2).map:
      case Seq(seed, count) =>
        check(seed, seed + count)
    .min
