package io.c6.aoc.y2022

import io.c6.aoc.util.BaseSolution
import io.c6.aoc.util.BaseSolution.*
import io.c6.aoc.util.Day.*
import io.c6.aoc.util.InputType.*
import io.c6.aoc.util.Year.*

object Day01 extends BaseSolution:
  protected override val part1InputFileName: String = getInputFileName(_2022, _01, A1)

  private case class Triplet(private val list: List[Int] = List.empty):
    val (a, b, c) = list match
      case a :: b :: c :: _ => (a, b, c)
      case a :: b :: Nil => (a, b, 0)
      case a :: Nil => (a, 0, 0)
      case Nil => (0, 0, 0)

    def replaceIfBigger(d: Int): Triplet =
      val m = min
      if d > m then
        if m == a then this.copy(List(d, b, c))
        else if m == b then this.copy(List(a, d, c))
        else if m == c then this.copy(List(a, b, d))
        else this
      else this

    def max: Int = Math.max(a, Math.max(b, c))

    def min: Int = Math.min(a, Math.min(b, c))

    def sum: Int = a + b + c

    override def toString: String = s"$a, $b, $c"

  private def mostCalories(input: Seq[String]): Triplet =
    val (triplet, acc) = input.foldLeft(Triplet() -> 0) { case pair -> line =>
      line match
        case "" => pair match
          case triplet -> acc => triplet.replaceIfBigger(acc) -> 0
        case s => pair match
          case triplet -> acc => triplet -> (acc + s.toInt)
    }
    triplet.replaceIfBigger(acc)

  protected override def part1Solution: Seq[String] => Unit = input =>
    val result = mostCalories(input).max
    println(s"Result for part 1: $result")

  protected override def part2Solution: Seq[String] => Unit = input =>
    val result = mostCalories(input).sum
    println(s"Result for part 2: $result")

@main def runDay01: Unit = Day01.run
