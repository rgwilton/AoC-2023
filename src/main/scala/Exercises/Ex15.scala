package aoc

object Ex15 extends Exercise:
  type Common = Array[String]

  def common(input: Iterator[String]) = 
    input.mkString("").split(",")

  def hash(s: String) =
    var v = 0
    for c <- s do
      v = ((v + c) * 17) % 256
    v

  def part1(steps: Common) = 
    steps.map(hash).sum

  case class Lens(label: String, power: Int)
  def part2(steps: Common) =
    val boxes = Array.fill(256)(List[Lens]())
    for step <- steps do
      if step.last == '-' then
        val label = step.init
        val h = hash(label)
        boxes(h) =
          boxes(h).filterNot(l => l.label == label)
      else
        // Labels are stored in reverse direction.
        val Array(label, value) = step.split("=")
        val lens = Lens(label, value.toInt)
        val h = hash(label)
        boxes(h) = 
          val b = boxes(h)
          b.indexWhere(l => l.label == label)
          match
            case -1 => lens :: b
            case x => b.updated(x, lens)
              //b.take(x) ::: List(lens) ::: b.drop(x + 1)
    
    // Calc
    (for (b, i) <- boxes.zip(Iterator.from(1))
      (l, j) <- b.reverse.zip(Iterator.from(1)) yield
        i * j * l.power
    ).sum
