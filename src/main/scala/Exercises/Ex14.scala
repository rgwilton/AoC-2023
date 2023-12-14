package aoc
import scala.collection.mutable

object Ex14 extends Exercise:
  type Common = Array[Array[Char]]

  def common(input: Iterator[String]) = 
    input.toArray.map(line => line.toArray)

  def rollNorth(grid: Common) =
    for y <- 1 until grid.length do
      for x <- 0 until grid(y).length do
        if grid(y)(x) == 'O' then
          var p = y - 1
          while p >= 0 && grid(p)(x) == '.' do
            p -= 1
          p += 1
          if p < y then
            grid(p)(x) = 'O'
            grid(y)(x) = '.'

  def rollSouth(grid: Common) =
    for y <- grid.length - 2 to 0 by -1 do
      for x <- 0 until grid(y).length do
        if grid(y)(x) == 'O' then
          var p = y + 1
          while p < grid.length && grid(p)(x) == '.' do
            p += 1
          p -= 1
          if p > y then
            grid(p)(x) = 'O'
            grid(y)(x) = '.'

  def rollWest(grid: Common) =
    for x <- 1 until grid(0).length do
      for y <- 0 until grid.length do
        if grid(y)(x) == 'O' then
          var p = x - 1
          while p >= 0 && grid(y)(p) == '.' do
            p -= 1
          p += 1
          if p < x then
            grid(y)(p) = 'O'
            grid(y)(x) = '.'

  def rollEast(grid: Common) =
    for x <- grid(0).length - 2 to 0 by -1 do
      for y <- 0 until grid.length do
        if grid(y)(x) == 'O' then
          var p = x + 1
          while p < grid(0).length && grid(y)(p) == '.' do
            p += 1
          p -= 1
          if p > x then
            grid(y)(p) = 'O'
            grid(y)(x) = '.'

  def printGrid(grid: Common) =
    grid.foreach: line =>
      println(line.mkString)
    println

  def cycle(grid: Common) = 
    rollNorth(grid)
    rollWest(grid)
    rollSouth(grid)
    rollEast(grid)

  def calcLoad(grid: Common) =
    (for y <- 0 until grid.length yield
      (for x <- 0 until grid(y).length yield
        if grid(y)(x) == 'O' then grid.length - y else 0
      ).sum
    ).sum

  // Convert the grid position into a number for easier comparison.
  def calcPos(grid: Common) =
    (for y <- 0 until grid.length yield
      (for x <- 0 until grid(y).length yield
        if grid(y)(x) == 'O' then (x.toLong * grid.length) + y else 0L
      ).sum
    ).sum

  def part1(grid: Common) = 
    rollNorth(grid)
    calcLoad(grid)

  def part2(grid: Common) = 
    // Cache the positions that we have already seen
    case class Seen(load: Int, cycles: Int)
    val seenMap = mutable.Map[Long, Seen]()
    var loadList = List[Int]()

    var cycles = 0
    var load = calcLoad(grid)
    var pos = calcPos(grid)
    while !seenMap.contains(pos) do
      seenMap += pos -> Seen(load, cycles)
      loadList = load :: loadList
      cycle(grid)
      cycles += 1
      load = calcLoad(grid)
      pos = calcPos(grid)
    
    val cycleLen = cycles - seenMap(pos).cycles
    val remCycles = (1_000_000_000 - cycles) % cycleLen

    val finalLoad = loadList.drop(cycleLen - remCycles - 1).head
    finalLoad