package aoc

import fastparse._, SingleLineWhitespace._

object Ex04 extends Exercise:
  type Common = Seq[Card]

  case class Card(no: Int, winning: Seq[Int], nos: Seq[Int]):
    val winners = (winning.toSet intersect nos.toSet).size

  object CardParser:
    // Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    def num[$: P]: P[Int] = P( CharsWhileIn("0-9").!.map(_.toInt) )
    def card[$: P] = P("Card" ~/ num ~/ ":" ~/ num.rep ~ "|" ~/ num.rep).map(Card.apply)
    def cards[$: P]: P[Seq[Card]] = P(card.rep ~ End)

  def common(input: Iterator[String]) = 
    parse(input.mkString, CardParser.cards(_)).get.value

  def part1(cards: Seq[Card]) = 
    cards.map:
      card =>
        if card.winners == 0 then 0 else 2.toPower(card.winners - 1).toInt
    .sum

  def part2(cards: Seq[Card]) = 
    class CardCopies(val card: Card, var copies: Long)
    val copies = cards.map(CardCopies(_, 1)).toArray
    copies.map:
      cc =>
        for i <- 1 to cc.card.winners do
          copies(cc.card.no + i - 1).copies += cc.copies
        cc.copies
    .sum