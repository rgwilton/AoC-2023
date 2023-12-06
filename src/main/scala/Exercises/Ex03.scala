package aoc

object Ex03 extends Exercise:
  type Common = Array[String]

  def common(input: Iterator[String]) = 
    def wrapped(s: String) = "." ++ s ++ "."
    val first = input.next
    val len = first.length
    val emptyStr = String(Array.fill(len + 2)('.'))
    (Seq(emptyStr, wrapped(first)) ++ input.map(wrapped)  ++ Seq(emptyStr)).toArray

  def part1(input: Array[String]) = 
    val lineLen = input(0).length
    var sum = 0
    for y <- 1 until input.length - 1 yield
      var inNum = false
      var curNum = 0
      var curNumStartIndex = 0
      for x <- 1 to lineLen - 1 yield
        if input(y)(x).isDigit then
          inNum = true
          if curNum > 0 then
            curNum = curNum * 10 + (input(y)(x) - '0')
          else
            curNum = input(y)(x) - '0'
            curNumStartIndex = x
        else
          if inNum then
            val curNumEndIndex = x - 1

            // Check if num is touching a symbol.
            var touching = false
              for x2 <- curNumStartIndex - 1 to curNumEndIndex + 1 do
                if !(input(y - 1)(x2) == '.') then touching = true
              for x2 <- curNumStartIndex - 1 to curNumEndIndex + 1 do
                if !(input(y + 1)(x2) == '.') then touching = true
              if input(y)(curNumStartIndex - 1) != '.' then touching = true
              if input(y)(curNumEndIndex + 1) != '.' then touching = true

            // for y2 <- y - 1 to y + 1 do
            //   for x2 <- curNumStartIndex - 1 to curNumEndIndex + 1 do
            //     val ch = input(y2)(x2)
            //     if !(ch.isDigit || ch == '.') then touching = true
            if touching then 
              sum += curNum
            else
            inNum = false
            curNum = 0
    sum

  def part2(input: Common) =
    val lineLen = input(0).length
    var sum = 0
    for y <- 1 until input.length - 1 yield
      var inNum = false
      var curNum = 0
      var curNumStartIndex = 0
      for x <- 1 until lineLen - 1 yield
        val ch = input(y)(x)
        if ch != '.' && !ch.isDigit then
          var c = List[(Int, Int)]()
          if input(y)(x - 1).isDigit then 
            var s = x - 1
            while input(y)(s - 1).isDigit do
              s -= 1
            c ::= (y, s)

          if input(y)(x + 1).isDigit then 
            c ::= (y, x + 1)

          if input(y - 1)(x).isDigit then 
            var s = x
            while input(y - 1)(s - 1).isDigit do
              s -= 1
            c ::= (y - 1, s)
          else 
            if input(y - 1)(x - 1).isDigit then 
              var s = x - 1
              while input(y - 1)(s - 1).isDigit do
                s -= 1
              c ::= (y - 1, s)
            if input(y - 1)(x + 1).isDigit then 
              c ::= (y - 1, x + 1)

          if input(y + 1)(x).isDigit then 
            var s = x
            while input(y + 1)(s - 1).isDigit do
              s -= 1
            c ::= (y + 1, s)
          else 
            if input(y + 1)(x - 1).isDigit then 
              var s = x - 1
              while input(y + 1)(s - 1).isDigit do
                s -= 1
              c ::= (y + 1, s)
            if input(y + 1)(x + 1).isDigit then 
              c ::= (y + 1, x + 1) 
          if c.length == 2 then
            //println(c)
            def mkNum(y: Int, x: Int) =
              var sum = 0
              var x2 = x
              var ch = input(y)(x2)
              while ch.isDigit do
                sum = sum * 10 + ch - '0'
                x2 += 1
                ch = input(y)(x2)
              sum
            sum += mkNum.tupled(c(0)) * mkNum.tupled(c(1)) 
    sum
