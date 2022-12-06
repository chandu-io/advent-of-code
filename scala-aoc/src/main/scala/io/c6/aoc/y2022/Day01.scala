package io.c6.aoc.y2022

import scala.io.{BufferedSource, Source}

object Day01:

  //private val input: BufferedSource = Source.fromResource("input/2022-day-01-sample-input.txt")
  private val input: BufferedSource = Source.fromResource("input/2022-day-01-input.txt")

  private case class Triplet(private val list: List[Int] = List.empty) {
    val (a, b, c) = list match
      case a :: b :: c :: _ => (a, b, c)
      case a :: b :: Nil => (a, b, 0)
      case a :: Nil => (a, 0, 0)
      case Nil => (0, 0, 0)

    def replaceIfBigger(d: Int): Triplet =
      val m = min
      if (d > m)
        if (m == a) this.copy(List(d, b, c))
        else if (m == b) this.copy(List(a, d, c))
        else if (m == c) this.copy(List(a, b, d))
        else this
      else this

    def max: Int = Math.max(a, Math.max(b, c))

    def min: Int = Math.min(a, Math.min(b, c))

    def sum: Int = a + b + c

    override def toString: String = s"$a, $b, $c"
  }

  private def mostCalories(inputSeq: Seq[String]): Triplet =
    val last = inputSeq.scanLeft((Triplet(), 0)) { (pair, line) =>
      line match
        case "" => pair match
          case (triplet, acc) => (triplet.replaceIfBigger(acc), 0)
        case s => pair match
          case (triplet, acc) => (triplet, acc + s.toInt)
    }.last
    last._1.replaceIfBigger(last._2)

  @main def solutionY2022Day01: Unit =
    val inputSeq = input.getLines().toSeq

    val result = mostCalories(inputSeq)
    println(s"Top 3 Elves carrying most calories    : $result")

    val result1 = result.max
    println(s"Elf carrying most calories            : $result1")

    val result2 = result.sum
    println(s"Total calories carried by top 3 Elves : $result2")

    input.close
