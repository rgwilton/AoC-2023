package aoc

object Ex10 extends Exercise:
  case class Common(grid: Array[Array[Char]], path: List[(Int, Int)])
  case class Move(x: Int, y: Int, nextX: Int, nextY: Int)
  enum Direction:
    case North, South, East, West
    
  import Direction._
  def charToDirs(ch: Char) = 
    ch match
      case 'J' => Set(North, West)
      case 'L' => Set(North, East)
      case '|' => Set(North, South)
      case '7' => Set(South, West)
      case 'F' => Set(South, East)
      case '-' => Set(East, West)
      case 'S' => Set(North, South, East, West)
      case '.' => Set()

  def opposite(d: Direction) =
    d match
      case North => South
      case South => North
      case West => East
      case East => West

  def common(input: Iterator[String]) = 
    val grid = input.map(_.toArray).toArray.reverse

    var (startX, startY) = (0, 0)
    for y <- 0 until grid.length
        x <- 0 until grid(0).length do
      if grid(y)(x) == 'S' then
        startX = x
        startY = y

    def checkNext(x: Int, y: Int, d: Direction) =
      d match
        case North if y < grid.length - 1 =>
          val dirs = charToDirs(grid(y + 1)(x))
          if dirs.contains(South) then
            Some(x, y + 1, (dirs - South).head)
          else
            None
        case South if y > 0 =>
          val dirs = charToDirs(grid(y - 1)(x))
          if dirs.contains(North) then
            Some(x, y - 1, (dirs - North).head)
          else
            None
        case East if x < grid(0).length - 1 =>
          val dirs = charToDirs(grid(y)(x + 1))
          if dirs.contains(West) then
            Some(x + 1, y, (dirs - West).head)
          else
            None
        case West if x > 0 =>
          val dirs = charToDirs(grid(y)(x - 1))
          if dirs.contains(East) then
            Some(x - 1, y, (dirs - East).head)
          else
            None
        case _ => None

    def check(x: Int, y: Int, d: Direction, path: List[(Int, Int)]): Option[List[(Int, Int)]] =
      if x == startX && y == startY && path.nonEmpty then
        return Some((x, y) :: path)
      else
        checkNext(x, y, d) match
          case Some(newX, newY, newD) => check(newX, newY, newD, (x, y) :: path)
          case None => None

    val path = 
      Direction.values.iterator.flatMap: direction =>
        check(startX, startY, direction, List())
      .next
    Common(grid, path)

  def part1(common: Common) = 
    common.path.length / 2
    
  def part2(common: Common) =
    val grid = common.grid
    val pathSet = common.path.toSet
    var area = 0
    for y <- 0 until grid.length do
      var inside = false
      for x <- 0 until grid(0).length do
        if pathSet.contains((x, y)) then
          if charToDirs(grid(y)(x)).contains(South) then
            inside = !inside
        else if inside then
          area += 1
    area

        