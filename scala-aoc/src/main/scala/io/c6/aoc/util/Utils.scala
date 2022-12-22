package io.c6.aoc.util

import scala.concurrent.duration.{Duration, NANOSECONDS}

object Utils:
  def time[T](operation: => T): T =
    val startTime = System.nanoTime()
    val result = operation
    val elapsedDuration = Duration(System.nanoTime() - startTime, NANOSECONDS)
    println(s"Time taken: ${elapsedDuration.toMillis} milliseconds")
    result
