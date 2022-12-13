package io.c6.aoc.y2022

import io.c6.aoc.BaseSolution
import io.c6.aoc.Day.*
import io.c6.aoc.Year.*
import io.c6.aoc.InputType.*

import scala.io.{BufferedSource, Source}

object Day03 extends BaseSolution:

  protected override val part1InputFileName: String = BaseSolution.getInputFileName(_2022, _03, A1)
  protected override val part2InputFileName: String = BaseSolution.getInputFileName(_2022, _03, A2)

  private val priority = ('a' to 'z').appendedAll('A' to 'Z').zipWithIndex.toMap

  protected override def part1Solution: Seq[String] => Unit = { input =>
    val result = input.filter(_.nonEmpty).map { s =>
      val n = s.length / 2
      // split the line into equal halves
      val (a, b) = s.zipWithIndex.partition { case (_, i) => i < n }
      // find the common item int the two halves and find the priority (zero-based) and add 1
      a.map(_._1).toSet.intersect(b.map(_._1).toSet).lastOption.map(priority).map(_ + 1).getOrElse(0)
    }.sum
    println(s"Result for part 1: $result")
  }

  protected override def part2Solution: Seq[String] => Unit = { input =>
    val result = input.filter(_.nonEmpty).grouped(3).map { s =>
      s.map(_.toSet).reduce(_.intersect(_)).lastOption.map(priority).map(_ + 1).getOrElse(0)
    }.sum
    println(s"Result for part 2: $result")
  }

@main def runDay03: Unit = Day03.run
