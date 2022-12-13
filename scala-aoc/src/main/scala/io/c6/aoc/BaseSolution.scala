package io.c6.aoc

import scala.concurrent.duration.{Duration, NANOSECONDS}
import scala.io.{BufferedSource, Source}

private trait BaseSolution extends Runnable {

  //noinspection JavaMutatorMethodOverriddenAsParameterless
  override def run: Unit = BaseSolution.time {
    val bufferedSource = Source.fromResource(inputFileName)
    val inputSeq = bufferedSource.getLines().toSeq
    solution(inputSeq)
    bufferedSource.close
  }

  protected def inputFileName: String

  protected def solution: Seq[String] => Unit
}

object BaseSolution {
  private def time[T](operation: => T): T = {
    val startTime = System.nanoTime()
    val result = operation
    val elapsedDuration = Duration(System.nanoTime() - startTime, NANOSECONDS)
    println(s"Time taken: ${elapsedDuration.toMillis} milliseconds")
    result
  }

  def getInputFileName(year: Year, day: Day, sampleInput: Boolean = false): String = {
    val suffix = if (sampleInput) "sample-input" else "input"
    s"input/${year.value}-day-${day.value}-$suffix.txt"
  }
}
