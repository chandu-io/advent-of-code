package io.c6.aoc.y2022

import io.c6.aoc.BaseSolution
import io.c6.aoc.Day.*
import io.c6.aoc.Year.*
import io.c6.aoc.InputType.*

import scala.io.{BufferedSource, Source}

object Day01 extends BaseSolution:

  protected override val part1InputFileName: String = BaseSolution.getInputFileName(_2022, _01, A1)

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

  private def mostCalories(input: Seq[String]): Triplet =
    val last = input.scanLeft((Triplet(), 0)) { (pair, line) =>
      line match
        case "" => pair match
          case (triplet, acc) => (triplet.replaceIfBigger(acc), 0)
        case s => pair match
          case (triplet, acc) => (triplet, acc + s.toInt)
    }.last
    last._1.replaceIfBigger(last._2)

  protected override def part1Solution: Seq[String] => Unit = { input =>
    val result = mostCalories(input).max
    println(s"Result for part 1: $result")
  }

  protected override def part2Solution: Seq[String] => Unit = { input =>
    val result = mostCalories(input).sum
    println(s"Result for part 2: $result")
  }

@main def runDay01: Unit = Day01.run
