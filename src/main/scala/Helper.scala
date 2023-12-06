package aoc

import util.chaining.scalaUtilChainingOps
import scala.annotation.tailrec

// Split a string into an iterator of integers, one per line.
extension (str: Iterator[String])
  def asIntegers: Iterator[Int] = str.map(_.toInt)

extension (lines: Iterator[String])
  def splitByEmptyLine: IndexedSeq[IndexedSeq[String]] =
    val builder = IndexedSeq.newBuilder[IndexedSeq[String]]

    @tailrec
    def worker(lines: Iterator[String]): Unit =
      val first = lines.takeWhile(_ != "")
      builder += first.toVector
      if lines.hasNext then worker(lines)
    worker(lines)
    builder.result()

extension (str: Array[String])
  def asIntegers: Array[Int] = str.map(_.toInt)

extension (str: IndexedSeq[String])
  def asIntegers: IndexedSeq[Int] = str.map(_.toInt)

extension (str: String)
  def asIntegers: Array[Int] = str.split(",").asInstanceOf[Array[String]].asIntegers

extension (x: Int)
  def hasBitSet(index: Int): Boolean =
    inline def mask = 1 << index
    (x & mask) != 0

  def withBitSet(index: Int): Int =
    x | (1 << index)

extension (x: Long)
  def isEven = x % 2 == 0
  def isOdd = x % 2 == 1
  def toPower(exp: Int) = scala.math.pow(x.toDouble, exp).toLong

extension (xs: Seq[Int])
  def mean = xs.sum/xs.length
  def median = 
    val sortedXs = xs.sorted
    sortedXs(xs.size / 2)

extension (xs: Seq[Long])
  def mean = xs.sum/xs.length
  def median = 
    val sortedXs = xs.sorted
    sortedXs(xs.size / 2)

def sumArrayInPlace(a: Array[Int], b: Array[Int]) = 
  for i <- 0 until a.length do a(i) += b(i)
  a

def sumArray(a: Array[Int], b: Array[Int]) = 
  (for i <- 0 until a.length yield a(i) + b(i)).toArray

// Add a pipe operator
extension [A,B](a: A)
  def  |> (f: (A) => B): B = a.pipe(f)


def measure[R](fn: => R): (R, Double) =
  val t1 = System.nanoTime
  val res = fn
  val t2 = System.nanoTime
  (res, (t2-t1).toDouble/1000000)

def gcd(a: Int, b: Int):Int = if (b == 0) a.abs else gcd(b, a % b)
def lcm(a: Int, b: Int) = (a * b).abs / gcd(a, b)