package io.c6.aoc.y2022

import io.c6.aoc.util.BaseSolution
import io.c6.aoc.util.Day.*
import io.c6.aoc.util.InputType.*
import io.c6.aoc.util.Year.*

@main def _2022_10: Unit = Day10(Actual)

object Day10 extends BaseSolution(_2022, _10):
  private val width = 40
  private val height = 6
  private val signalStrengthCycles = LazyList.from(20, width).take(height)
  private val cycles = 1 to (width * height)

  private def computeRegisterRunningValues(instructions: Seq[String]) =
    instructions.flatMap { instruction =>
      if instruction == "noop" then Seq(0) else Seq(0, instruction.substring(5).toInt)
    }.scanLeft(1)(_ + _)

  protected def part1Solution: Seq[String] => Unit = input =>
    val registerValues = computeRegisterRunningValues(input)
    val result = signalStrengthCycles.map(cycle => cycle * registerValues(cycle - 1)).sum
    println(s"Result for part 1: $result")

  protected def part2Solution: Seq[String] => Unit = input =>
    val registerValues = computeRegisterRunningValues(input)
    val result = cycles.map { cycle =>
      val pixel = (cycle - 1) % width
      if Set(pixel - 1, pixel, pixel + 1).contains(registerValues(cycle - 1)) then '#' else '.'
    }.grouped(width).map(_.mkString).mkString(System.lineSeparator())
    println(s"Result for part 2: ${System.lineSeparator()}$result")
