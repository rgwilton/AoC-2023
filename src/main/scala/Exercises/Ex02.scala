package aoc

import fastparse._, MultiLineWhitespace._

object Ex02 extends Exercise:
  type Common = Games

  case class Move(red: Int, green: Int, blue: Int):
    def +(m:Move) = Move(red + m.red, green + m.green, blue + m.blue)
    def max(m:Move) = Move(red max m.red, green max m.green, blue max m.blue)

  case class Game(no: Int, moves: Seq[Move]):
    val max = moves.reduce(_ max _)

  type Games = Seq[Game]

  object GamesParser:
    //Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    def number[$: P]: P[Int] = P( CharsWhileIn("0-9").!.map(_.toInt) )
    def colour[$: P]: P[Move] = P(number ~/ ("red".! | "green".! | "blue".!)).map:
      case (x, "red") => Move(x, 0, 0)
      case (x, "green") => Move(0, x, 0)
      case (x, "blue") => Move (0, 0, x)

    def move[$: P]: P[Move] = P(colour.rep(1, ",")).map:
      colours => colours.reduce(_ + _)

    def game[$: P] = P("Game " ~/ number ~/ ":" ~ move.rep(1, ";")).map:
      (id, moves) => Game(id, moves)

    def games[$: P]: P[Games] = P(game.rep ~ End)

  def common(input: Iterator[String]) =
    val res = parse(input.mkString, GamesParser.games(_))
    res.get.value

  def part1(games: Games) =
    games.filter:
      game =>
        def max = game.max
        max.red <= 12 && max.green <= 13 && max.blue <= 14 
    .map(_.no)
    .sum

  def part2(games: Games) =
    games.map:
      game => 
        def max = game.max
        max.blue * max.green * max.red
    .sum