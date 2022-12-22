package io.c6.aoc.y2021

import io.c6.aoc.util.BaseSolution
import io.c6.aoc.util.Day.*
import io.c6.aoc.util.InputType.*
import io.c6.aoc.util.Year.*

import scala.io.{BufferedSource, Source}

@main def _2021_01: Unit = Day01()

private object Day01 extends BaseSolution(_2021, _01):
  private def countDepthIncrements(depths: Seq[Int]): Int =
    depths.sliding(2).count {
      case a :: b :: Nil => b > a
      case _ => false
    }

  protected override def part1Solution: Seq[String] => Unit = input =>
    val depths = input.map(_.toInt)
    val result = countDepthIncrements(depths)
    println(s"Result for part 1: $result")

  protected override def part2Solution: Seq[String] => Unit = input =>
    val depthSums = input.map(_.toInt).sliding(3).map(_.sum).toSeq
    val result = countDepthIncrements(depthSums)
    println(s"Result for part 2: $result")
