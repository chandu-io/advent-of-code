package io.c6.aoc.y2022

import io.c6.aoc.util.BaseSolution
import io.c6.aoc.util.Day.*
import io.c6.aoc.util.InputType.*
import io.c6.aoc.util.Year.*

@main def _2022_03: Unit = Day03(Actual)

object Day03 extends BaseSolution(_2022, _03):

  private val priorityMap: Map[Char, Int] = ('a' to 'z').appendedAll('A' to 'Z')
    .zipWithIndex.toMap.transform { case _ -> p => p + 1 }

  extension (str: String)
    private def half: Seq[String] = str.grouped(str.length / 2).toSeq

  extension (seq: Seq[String])
    private def priority: Int =
      seq.reduce(_ intersect _).headOption.map(priorityMap).getOrElse(0)

  protected override def part1Solution: Seq[String] => Unit = input =>
    val result = input.map(_.half.priority).sum
    println(s"Result for part 1: $result")

  protected override def part2Solution: Seq[String] => Unit = input =>
    val result = input.grouped(3).map(_.priority).sum
    println(s"Result for part 2: $result")
