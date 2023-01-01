package io.c6.aoc.y2022

import io.c6.aoc.util.{BaseSolution, Direction, Grid}
import io.c6.aoc.util.Day.*
import io.c6.aoc.util.InputType.*
import io.c6.aoc.util.Year.*
import io.c6.aoc.util.Utils.StringExt.*
import io.c6.aoc.util.Utils.IterableExt.*
import io.c6.aoc.util.Utils.PairExt.*
import io.c6.aoc.util.Grid.*

import scala.concurrent.duration.{Duration, NANOSECONDS}

@main def _2022_14: Unit = Day14()

object Day14 extends BaseSolution(_2022, _14):

  private sealed trait Element

  private object Air extends Element:
    override def toString: String = "."

  private object Rock extends Element:
    override def toString: String = "#"

  private object Sand extends Element:
    override def toString: String = "O"

  private val sandPropagationOrder = Seq(Direction.S, Direction.SW, Direction.SE)

  private class Structure(sandStartPositionStr: String, paths: Seq[String], isPart1: Boolean):
    private val sandPosition = sandStartPositionStr.splitToPair(",").map(_.toInt).swap
    private val rockPositions = paths.flatMap {
      _.splitToSeq(" -> ").map(_.splitToPair(",").map(_.toInt))
        .sliding(2).map(_.toPair)
        .flatMap { (a, b) =>
          val (x, y) = (a.first -> b.first) -> (a.second -> b.second)
          if a.first == b.first then (y.min to y.max).map(a.first -> _)
          else if a.second == b.second then (x.min to x.max).map(_ -> a.second)
          else Seq(a, b)
        }.toSeq.distinct
    }.distinct.map(_.swap)
    private val (rowSeq, colSeq) = (sandPosition +: rockPositions).unzip
    private val rowRange = if isPart1 then rowSeq.min to rowSeq.max else rowSeq.min to rowSeq.max + 2
    private val colRange = if isPart1 then colSeq.min to colSeq.max else InfRange
    private val elementMap = Map.from((sandPosition -> Sand) +: rockPositions.map(_ -> Rock))
    private val grid = new Grid(elementMap, Air, false, PositionRange(rowRange, colRange))

    private def getNextPart1(position: Position): Option[Position] =
      grid(position).neighbours(sandPropagationOrder)
        .find(c => grid(c.position).data == Air).map(_.position)

    private def getNextPart2(position: Position): Option[Position] =
      grid(position).neighbours(sandPropagationOrder)
        .find(c => grid(c.position).data == Air && c.position.first != rowRange.end).map(_.position)

    lazy val simulate: Int =
      var done = false
      while !done do
        var currPosition = sandPosition
        var canMoveSouth = true
        while !done && canMoveSouth do
          val maybeNextPosition = if isPart1 then getNextPart1(currPosition) else getNextPart2(currPosition)
          maybeNextPosition match
            case Some(nextPosition) =>
              currPosition = nextPosition
            case None =>
              canMoveSouth = false
              grid(currPosition) = Sand
          done = !grid.withinRange(currPosition + Direction.SW.distance) || currPosition == sandPosition
      grid.count(_ == Sand)

  protected def part1Solution: Seq[String] => Unit = input =>
    val result = new Structure("500,0", input, true).simulate - 1
    println(s"Result for part 1: $result")

  protected def part2Solution: Seq[String] => Unit = input =>
    val result = new Structure("500,0", input, false).simulate
    println(s"Result for part 2: $result")
