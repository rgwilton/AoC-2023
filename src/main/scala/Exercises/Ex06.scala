package aoc

import fastparse._, MultiLineWhitespace._

object Ex06 extends Exercise:
  type Common = Seq[(Int, Int)]

  object Parser:
    //Time:      7  15   30
    //Distance:  9  40  200
    def num[$: P]: P[Int] = P( CharsWhileIn("0-9").!.map(_.toInt) )
    def time[$: P] = P("Time:" ~/ num.rep)
    def distance[$: P] = P("Distance:" ~/ num.rep)
    def race[$: P]  = P(time ~ distance ~ End).map:
      (times, races) => times zip races

  def common(input: Iterator[String]) = 
    parse(input.mkString("\n"), Parser.race(_)).get.value

  def part1(races: Common) = 
    races.map:
      (time, distance) =>
        (0 to time).map:
          t => (time - t) * t
        .count:
          t => t > distance
    .product

  def part2(races: Common) = 
    val (times, distances) = races.unzip
    val time = times.mkString.toLong
    val distance = distances.mkString.toLong

    def total(t: Long) = (time - t) * t

    val lowestBound =
      var t = 0L
      var interval = time/2
      while interval >= 1 do
        if total(t + interval) > distance then
          interval = interval / 2
        else
          t += interval
      t

    val upperBound =
      var t = time
      var interval = time/2
      while interval >= 1 do
        if total(t - interval) > distance then
          interval = interval / 2
        else
          t -= interval
      t

    upperBound - lowestBound - 1
