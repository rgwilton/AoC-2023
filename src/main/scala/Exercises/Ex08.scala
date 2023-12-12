package aoc

import fastparse._, MultiLineWhitespace._

object Ex08 extends Exercise:
  type Common = DMap

  case class Node(from: String, left: String, right: String)
  case class DMap(directions: String, nodes: Seq[Node]):
    val nodeMap = nodes.map(node => node.from -> node).toMap


  object Parser:
    // Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    def name[$: P]: P[String] = P(CharsWhileIn("A-Z0-9").!)
    def directions[$: P] = P(CharsWhileIn("LR").!)
    def node[$: P] = P(name ~/ "=" ~/ "(" ~/ name ~/ "," ~/ name ~ ")").map(Node.apply)
    def dmap[$: P] = P(directions ~/ node.rep ~ End).map(DMap.apply)

  def common(input: Iterator[String]) = 
    parse(input.mkString("\n"), Parser.dmap(_), verboseFailures = true).get.value

  def part1(dmap: DMap) = 0 
    // val steps = Iterator.continually(dmap.directions.iterator).flatten
    // var stepCount = 0
    // var loc = "AAA"
    // while loc != "ZZZ" do
    //   loc = 
    //     if (steps.next == 'L') then
    //       dmap.nodeMap(loc).left
    //     else
    //       dmap.nodeMap(loc).right
    //   stepCount += 1
    // stepCount

  def part2(dmap: DMap) = 0
    // def stepsIter = Iterator.continually(dmap.directions.iterator.zipWithIndex).flatten
    // var stepCount = 0
    // var seenMap = scala.collection.mutable.Map[(String, Int), (String, Int)]() 
    // val locs = dmap.nodes.map(_.from).filter(_.endsWith("A")).toArray
    // def isEnd(loc: String) = loc.endsWith("Z")
    // for loc <- locs do
    //   val si = stepsIter
    //   var curLoc = loc
    //   var step = si.next()
    //   var prevEnd = (curLoc, 0)
    //   var finished = false
    //   while !finished do
    //     // Find next end.
    //     var stepCount = 0
    //     while !isEnd(loc) do
    //       step = si.next()
    //       stepCount += 1
    //       curLoc = 
    //         if (step._1 == 'L') then
    //           dmap.nodeMap(loc).left
    //         else
    //           dmap.nodeMap(loc).right

    //     if !seenMap.contains(prevEnd) then
    //       seenMap += prevEnd -> (curLoc, stepCount)
    //     else
    //       finished = true

    //   while !isEnd(loc) && !seenMap.contains((loc, step._2)) do
    //     curLoc = 
    //       if (nextStep._1 == 'L') then
    //         dmap.nodeMap(loc).left
    //       else
    //         dmap.nodeMap(loc).right

    // while !reachedEnd && stepCount < 1000 do
    //   //println(locs.mkString(","))
    //   val nextStep = steps.next._1 == 'L'
    //   locs.mapInPlace:
    //     loc =>
    //       if (nextStep) then
    //         dmap.nodeMap(loc).left
    //       else
    //         dmap.nodeMap(loc).right
    //   stepCount += 1
    // stepCount

    // val steps = Iterator.continually(dmap.directions.iterator).flatten
    // var stepCount = 0
    // val locs = dmap.nodes.map(_.from).filter(_.endsWith("A")).toArray
    // println(locs.mkString(","))

    // def reachedEnd = locs.forall(_.endsWith("Z"))

    // while !reachedEnd && stepCount < 1000 do
    //   //println(locs.mkString(","))
    //   val nextStep = steps.next == 'L'
    //   locs.mapInPlace:
    //     loc =>
    //       if (nextStep) then
    //         dmap.nodeMap(loc).left
    //       else
    //         dmap.nodeMap(loc).right
    //   stepCount += 1
    // stepCount