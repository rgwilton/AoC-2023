package aoc

object Ex07 extends Exercise:
  type Common = Array[Hand]

  enum Hands:
    case HighCard, OnePair, TwoPairs, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind
  import Hands._

  def cardValue(card: Char) = 
    card match
      case 'A' => 14
      case 'K' => 13
      case 'Q' => 12
      case 'J' => 11
      case 'T' => 10
      case ch => ch - '0'    

  case class Hand(cards: String, bid: Int):
    val cardType = 
      val distinctCards = cards.distinct
      val counts = 
        distinctCards.map:
          card => cards.count(_ == card)
        .sorted(Ordering[Int].reverse)
      (counts(0), distinctCards.length) match
        case (5, 1) => FiveOfAKind
        case (4, 2) => FourOfAKind
        case (3, 2) => FullHouse
        case (3, 3) => ThreeOfAKind
        case (2, 3) => TwoPairs  
        case (2, 4) => OnePair
        case (1, 5) => HighCard

    val cardType2 = 
      val distinctCards = cards.filterNot(_ == 'J').distinct
      val jokers = cards.count(_ == 'J')
      if jokers == 5 then FiveOfAKind
      else
        val counts = 
          distinctCards.map:
            card => cards.count(_ == card)
          .sorted(Ordering[Int].reverse)
        (counts(0) + jokers, distinctCards.length) match
          case (5, 1) => FiveOfAKind
          case (4, 2) => FourOfAKind
          case (3, 2) => FullHouse
          case (3, 3) => ThreeOfAKind
          case (2, 3) => TwoPairs  
          case (2, 4) => OnePair
          case (1, 5) => HighCard
      
  def common(input: Iterator[String]) = 
    input.map:
      line =>
        val Array(cards, bid) = line.split(" ")
        Hand(cards, bid.toInt)
    .toArray

  def part1(hands: Array[Hand]) = 
    val sortedHands = 
      hands.sortWith:
        (h1, h2) => 
          if h1.cardType.ordinal != h2.cardType.ordinal then
            h1.cardType.ordinal <= h2.cardType.ordinal
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

  def part2(hands: Array[Hand]) = 
    def cardValue2(c: Char) = 
      if c == 'J' then 1 else cardValue(c)

    val sortedHands = 
      hands.sortWith:
        (h1, h2) => 
          if h1.cardType2.ordinal != h2.cardType2.ordinal then
            h1.cardType2.ordinal <= h2.cardType2.ordinal
          else 
            var i = 0
            while cardValue2(h1.cards(i)) == cardValue2(h2.cards(i)) do
              i += 1
            cardValue2(h1.cards(i)) < cardValue2(h2.cards(i))

    sortedHands
    .zip(Iterator.from(1))
    .map:
      (hand, rank) => hand.bid * rank
    .sum
