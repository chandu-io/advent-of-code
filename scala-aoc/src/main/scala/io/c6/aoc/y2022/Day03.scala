package io.c6.aoc.y2022

import io.c6.aoc.BaseSolution
import io.c6.aoc.BaseSolution.*
import io.c6.aoc.Day.*
import io.c6.aoc.Year.*
import io.c6.aoc.InputType.*

object Day03 extends BaseSolution:

  protected override val part1InputFileName: String = getInputFileName(_2022, _03, A1)
  protected override val part2InputFileName: String = getInputFileName(_2022, _03, A2)

  private val priorityMap: Map[Char, Int] = ('a' to 'z').appendedAll('A' to 'Z')
    .zipWithIndex.toMap.transform { case _ -> p => p + 1 }

  extension (str: String)
    private inline def half: Seq[String] =
      val n = str.length / 2
      val (leftHalf, rightHalf) = str.zipWithIndex.partition { case _ -> i => i < n }
      Seq(leftHalf.map(_._1).mkString, rightHalf.map(_._1).mkString)

  extension (seq: Seq[String])
    private inline def priority: Int =
      seq.reduce(_ intersect _).headOption.map(priorityMap).getOrElse(0)

  protected override def part1Solution: Seq[String] => Unit = { input =>
    val result = input.map(_.half.priority).sum
    println(s"Result for part 1: $result")
  }

  protected override def part2Solution: Seq[String] => Unit = { input =>
    val result = input.grouped(3).map(_.priority).sum
    println(s"Result for part 2: $result")
  }

@main def runDay03: Unit = Day03.run
