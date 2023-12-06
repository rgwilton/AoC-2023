package aoc

import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Success
import scala.util.Failure

@main def aoc(testNos: String*): Unit = 
  val AllExercises = 
    IndexedSeq(Ex01, Ex02, Ex03, Ex04, Ex05, Ex06, Ex07, Ex08, Ex09, Ex10,
               Ex11, Ex12, Ex13, Ex14, Ex15, Ex17, Ex18, Ex19, Ex20, Ex21, Ex22, Ex23, Ex24, Ex25).take(25)
  val loops = 100  // To measure jitted performance.s

  val validTestNos = testNos.flatMap(_.toIntOption).filter(_ <= AllExercises.length)

  val exercises = 
    if validTestNos.isEmpty then AllExercises
    else validTestNos.map(x => AllExercises(x - 1))

  def exRuns = 
    Future.sequence {
      exercises.map { ex =>
        Future {
          val runs = for i <- 1 to loops yield ex.run
          runs.last
        }
      }
    }.andThen {
      case Success(results) =>
        for Result(name, p1, p1t, p2, p2t, commonTime, totalTime) <- results do 
          val p1time =  commonTime + p1t
          val p2time = commonTime + p2t
          val commonStr = if commonTime > 0.1 then f", common: $commonTime%.1f ms" else ""

          println(f"""$name%-5s => part 1: "$p1", $p1time%.1f ms; part 2: "$p2", $p2time%.1f ms; """ +
                  commonStr +  f", total: $totalTime%.1f ms")
      case Failure(e) => throw e
    }

  Await.ready(exRuns, 10 minutes)


case class Result(name: String, part1: Any, p1Time: Double, part2: Any, p2Time: Double, commonTime: Double, totalTime: Double)

trait Exercise:
  type Common
  val name = getClass.getSimpleName.nn.init
  val num = name.drop(2)
  def input = scala.io.Source.fromFile(s"input/input_${num}.txt").getLines

  def common(input: Iterator[String]): Common
  def part1(commonRes: Common): Any
  def part2(commonRes: Common): Any

  def run: Result = run(input)
  def run(input: Iterator[String]): Result =
    measure {
      val (commonRes, commonTime) = measure { common(input) }
      (measure { part1(commonRes) },  measure { part2(commonRes) }, commonTime)
    } match
      case (((pt1, pt1Time), (pt2, pt2Time), commonTime), totalTime) =>
        Result(name, pt1, pt1Time, pt2, pt2Time, commonTime, totalTime)
  
