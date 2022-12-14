package io.c6.aoc.y2022

import io.c6.aoc.util.BaseSolution
import io.c6.aoc.util.Day.*
import io.c6.aoc.util.InputType.*
import io.c6.aoc.util.Year.*

@main def _2022_06: Unit = Day06()

object Day06 extends BaseSolution(_2022, _06):
  private def startOfPacket(message: String, markerSize: Int): Int =
    message.sliding(markerSize).indexWhere(_.toSet.size == markerSize) + markerSize

  override protected def part1Solution: Seq[String] => Unit = input =>
    val result = startOfPacket(input.head, 4)
    println(s"Result for part 1: $result")

  override protected def part2Solution: Seq[String] => Unit = input =>
    val result = startOfPacket(input.head, 14)
    println(s"Result for part 1: $result")
