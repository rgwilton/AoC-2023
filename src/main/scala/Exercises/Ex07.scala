package aoc

object Ex07 extends Exercise:
  type Common = Array[String]

  enum Hands:
    case HighCard, OnePair, TwoPairs, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind
  import Hands._

  case class Rules(jackIsJoker: Boolean = false)

  def cardValue(card: Char)(using r:Rules) = 
    card match
      case 'A' => 14
      case 'K' => 13
      case 'Q' => 12
      case 'J' => if r.jackIsJoker then 1 else 11
      case 'T' => 10
      case ch => ch - '0'    

  case class Hand(cards: String, bid: Int)(using r:Rules):
    val handType = 
      val (jokers, distinctCards) = 
        if r.jackIsJoker then
          (cards.count(_ == 'J'), cards.filterNot(_ == 'J').distinct)
        else (0, cards.distinct)
      val mostFreqCount = 
        if distinctCards.length == 0 then 0
        else
          distinctCards.map:
            card => cards.count(_ == card)
          .sorted(Ordering[Int].reverse).head
      (mostFreqCount + jokers, distinctCards.length) match
        case (5, _) => FiveOfAKind
        case (4, 2) => FourOfAKind
        case (3, 2) => FullHouse
        case (3, 3) => ThreeOfAKind
        case (2, 3) => TwoPairs  
        case (2, 4) => OnePair
        case (1, 5) => HighCard
      
  def parseCards(input: Seq[String])(using Rules) =
    input.map: line =>
      val Array(cards, bid) = line.split(" ")
      Hand(cards, bid.toInt)

  def common(input: Iterator[String]) = input.toArray

  def calcHandsSum(hands: Seq[Hand])(using Rules) = 
    val sortedHands = 
      hands.sortWith:
        (h1, h2) => 
          if h1.handType.ordinal != h2.handType.ordinal then
            h1.handType.ordinal <= h2.handType.ordinal
          else 
            var i = 0
            while cardValue(h1.cards(i))  == cardValue(h2.cards(i)) do
              i += 1
            cardValue(h1.cards(i)) < cardValue(h2.cards(i))
    sortedHands
    .zip(Iterator.from(1))
    .map:
      (hand, rank) => hand.bid * rank
    .sum

  def part1(input: Array[String]) = 
    given Rules(jackIsJoker = false)
    val hands = parseCards(input)
    calcHandsSum(hands)

  def part2(input: Array[String]) = 
    given Rules(jackIsJoker = true)
    val hands = parseCards(input)
    calcHandsSum(hands)