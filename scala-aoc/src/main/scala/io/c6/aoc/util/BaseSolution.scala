package io.c6.aoc.util

import scala.concurrent.duration.{Duration, NANOSECONDS}
import scala.io.{BufferedSource, Source}

trait BaseSolution extends Runnable:

  //noinspection JavaMutatorMethodOverriddenAsParameterless
  override def run: Unit = BaseSolution.time {
    // solve part 1
    val bufferedSource1 = Source.fromResource(part1InputFileName)
    part1Solution(bufferedSource1.getLines().toSeq)
    bufferedSource1.close
    // solve part 2
    val bufferedSource2 = Source.fromResource(part2InputFileName)
    part2Solution(bufferedSource2.getLines().toSeq)
    bufferedSource2.close
  }

  protected def part1InputFileName: String

  protected def part2InputFileName: String = part1InputFileName

  protected def part1Solution: Seq[String] => Unit

  protected def part2Solution: Seq[String] => Unit

object BaseSolution:
  private def time[T](operation: => T): T =
    val startTime = System.nanoTime()
    val result = operation
    val elapsedDuration = Duration(System.nanoTime() - startTime, NANOSECONDS)
    println(s"Time taken: ${elapsedDuration.toMillis} milliseconds")
    result

  def getInputFileName(year: Year, day: Day, inputType: InputType): String =
    s"input/${year.value}/${year.value}-day-${day.value}-${inputType.suffix}.txt"
