package io.c6.aoc.y2022

import io.c6.aoc.util.BaseSolution
import io.c6.aoc.util.BaseSolution.*
import io.c6.aoc.util.Day.*
import io.c6.aoc.util.InputType.*
import io.c6.aoc.util.Year.*

import scala.language.implicitConversions

object Day04 extends BaseSolution:
  override protected def part1InputFileName: String = getInputFileName(_2022, _04, A1)

  private type Assignment = (Int, Int)
  private type AssignmentPair = (Assignment, Assignment)

  private val nonOverlappingAssignmentPair = (0 -> 1, 2 -> 3)

  private def overlaps(assignmentPair: AssignmentPair): Boolean = assignmentPair match
    case (a -> b, x -> y) => x >= a && y <= b || a >= x && b <= y

  private def partiallyOverlaps(assignmentPair: AssignmentPair): Boolean = assignmentPair match
    case (a -> b, x -> y) => !(b < x || y < a)

  given Conversion[String, AssignmentPair] = str => str.split(',') match
    case Array(assignment1, assignment2) => assignment1.split('-') -> assignment2.split('-') match
      case Array(a, b) -> Array(x, y) => (a.toInt -> b.toInt, x.toInt -> y.toInt)
      case _ => nonOverlappingAssignmentPair
    case _ => nonOverlappingAssignmentPair

  override protected def part1Solution: Seq[String] => Unit = input =>
    val result = input.count(overlaps(_))
    println(s"Result for part 1: $result")

  override protected def part2Solution: Seq[String] => Unit = input =>
    val result = input.count(partiallyOverlaps(_))
    println(s"Result for part 1: $result")

@main def runDay04: Unit = Day04.run
