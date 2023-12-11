package aoc

object Ex11 extends Exercise:
  case class Pos(x: Int, y: Int)
  case class StarMap(maxX: Int, maxY: Int, positions: Seq[Pos])
  type Common = StarMap

  def common(input: Iterator[String]) = 
    val grid = input.toIndexedSeq
    val positions = 
      (0 until grid.length).iterator.flatMap: y =>
        (0 until grid(0).length).iterator.flatMap: x =>
          grid(y)(x) match
            case '#' => Some(Pos(x, y))
            case _ => None
      .toSeq
    StarMap(grid(0).length, grid.length, positions)

  def expand2d(starMap: StarMap, expRate: Int) =
    def expand1d(starMap: StarMap, dim: Pos => Long, max: Int) =
      val filledDim = starMap.positions.map(dim).toSet
      var expansion = 0 
      (0 until max).map: d =>
        if !filledDim.contains(d) then
          expansion += expRate
        expansion

    val xExpansion = expand1d(starMap, _.x, starMap.maxX)
    val yExpansion = expand1d(starMap, _.y, starMap.maxY)

    // Return new positions
    starMap.positions.map:
      p => Pos(p.x + xExpansion(p.x), p.y + yExpansion(p.y))

  def calcPairs(pos: Seq[Pos]) = 
    val allDistances = 
      for fp <- pos
          sp <- pos yield
        ((fp.x - sp.x).abs + (fp.y - sp.y).abs).toLong
    allDistances.sum / 2

  def part1(starMap: StarMap) =
    calcPairs(expand2d(starMap, 1))

  def part2(starMap: StarMap) =
    // Note 999,999 because it replaces existing empty row/column.
    calcPairs(expand2d(starMap, 999_999))