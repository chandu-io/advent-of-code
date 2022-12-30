package io.c6.aoc.y2022

import io.c6.aoc.util.BaseSolution
import io.c6.aoc.util.Day.*
import io.c6.aoc.util.InputType.*
import io.c6.aoc.util.Year.*
import io.c6.aoc.util.Utils.TripletImplicits.*
import io.c6.aoc.util.Utils.TripletExt.*

@main def _2022_01: Unit = Day01()

object Day01 extends BaseSolution(_2022, _01):
  private type Triplet = (Int, Int, Int)
  extension (triplet: Triplet)
    private def replaceIfBigger(d: Int): Triplet =
      val m = triplet.min
      if d > m then
        if m == triplet.first then d <> triplet.second <> triplet.third
        else if m == triplet.second then triplet.first <> d <> triplet.third
        else if m == triplet.third then triplet.first <> triplet.second <> d
        else triplet
      else triplet

  private def mostCalories(input: Seq[String]): Triplet =
    val (triplet, acc) = input.foldLeft((0, 0, 0) -> 0) { (pair, line) =>
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
    val result = mostCalories(input).toSeq.sum
    println(s"Result for part 2: $result")
