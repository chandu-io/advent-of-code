package io.c6.aoc.y2022

import io.c6.aoc.BaseSolution
import io.c6.aoc.BaseSolution.*
import io.c6.aoc.Day.*
import io.c6.aoc.InputType.*
import io.c6.aoc.Year.*

object Day06 extends BaseSolution:
  override protected def part1InputFileName: String = getInputFileName(_2022, _06, A1)

  private def startOfPacket(message: String, markerSize: Int): Int =
    message.sliding(markerSize).indexWhere(_.toSet.size == markerSize) + markerSize

  override protected def part1Solution: Seq[String] => Unit = { input =>
    val result = startOfPacket(input.head, 4)
    println(s"Result for part 1: $result")
  }

  override protected def part2Solution: Seq[String] => Unit = { input =>
    val result = startOfPacket(input.head, 14)
    println(s"Result for part 1: $result")
  }

@main def runDay06: Unit = Day06.run
