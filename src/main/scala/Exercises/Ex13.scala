package aoc

object Ex13 extends Exercise:
  type Common = IndexedSeq[IndexedSeq[String]]

  def common(input: Iterator[String]) = 
    input.splitByEmptyLine

  def mirror(block: IndexedSeq[String]) =
    def reflectsAt(x: Int) = 
      val (f, l) = block.splitAt(x)
      f.reverse.zip(l).forall((a, b) => a == b)
    (1 until block.length).find: i => 
      reflectsAt(i)
    .getOrElse(0)

  def smudgedMirror(block: IndexedSeq[String]) =
    def reflectsAt(x: Int) = 
      val (f, l) = block.splitAt(x)
      var smudge = false
      def checkSmudge(s1: String, s2: String) =
        val matchIfSmudged = 
          s1.zip(s2).count((c1, c2) => c1 == c2) == s1.length - 1
        if matchIfSmudged then smudge = true
        matchIfSmudged
      val reflection = 
        f.reverse.zip(l).forall:
          (l1, l2) =>
            l1 == l2 || checkSmudge(l1, l2)
      reflection && smudge
    (1 until block.length).find: i => 
      reflectsAt(i)
    .getOrElse(0)

  def rotate(block: IndexedSeq[String]) =
    for x <- 0 until block(0).length yield
      (0 until block.length).map: y => 
        block(y)(x)
      .mkString

  def part1(blocks: Common) =
    blocks.map: block =>
      val m = mirror(block)
      if m > 0 then 
        100 * m
      else
        mirror(rotate(block))
    .sum

  def part2(blocks: Common) = 
    blocks.map: block =>
      val m = smudgedMirror(block)
      if m > 0 then 
        100 * m
      else
        smudgedMirror(rotate(block))
    .sum