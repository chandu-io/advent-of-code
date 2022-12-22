package io.c6.aoc.util

import io.c6.aoc.util.InputType.*

import scala.concurrent.duration.{Duration, NANOSECONDS}
import scala.io.{BufferedSource, Source}
import scala.util.Using

trait BaseSolution(year: Year, day: Day) extends Function[Seq[InputType], Unit]:

  protected def part1Solution: Seq[String] => Unit

  protected def part2Solution: Seq[String] => Unit

  private def getInputFileName(suffix: String): String =
    s"input/${year.value}/${year.value}-day-${day.value}-$suffix.txt"

  override def apply(inputTypes: InputType*): Unit =
    val (res1, res2) = inputTypes.toList match
      case part1InputType :: part2InputType :: _ =>
        val part1InputFileName = part1InputType match
          case `Actual` => getInputFileName("input")
          case _ => getInputFileName("sample-input")
        val part2InputFileName = part2InputType match
          case `Actual` => getInputFileName("input-part2")
          case _ => getInputFileName("sample-input-part2")
        part1InputFileName -> part2InputFileName
      case `Actual` :: Nil =>
        val actualFileName = getInputFileName("input")
        actualFileName -> actualFileName
      case _ =>
        val sampleFileName = getInputFileName("sample-input")
        sampleFileName -> sampleFileName

    val part1Input = Using(Source.fromResource(res1))(_.getLines().toSeq)
    val part2Input = Using(Source.fromResource(res2))(_.getLines().toSeq)

    Utils.time {
      part1Input.fold(e => Console.err.println(s"Unable to solve part 1: $e"), part1Solution)
      part2Input.fold(e => Console.err.println(s"Unable to solve part 2: $e"), part2Solution)
    }
