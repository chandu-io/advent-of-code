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

  private def stepPart1(structure: Grid[Element]): Position => Option[Position] = position =>
    structure(position).neighbours(sandPropagationOrder)
      .find(c => structure(c.position).data == Air).map(_.position)

  private def stepPart2(structure: Grid[Element], last: Int): Position => Option[Position] = position =>
    val maybeNextCell = structure(position).neighbours(sandPropagationOrder)
      .find(c => structure(c.position).data == Air && c.position.first != last)
    maybeNextCell.map(_.position)

  private def parse(
                     sandStartPositionStr: String,
                     paths: Seq[String],
                   ): (Position, Map[Position, Element], Range, Range) =
    val rockPositions = paths.flatMap {
      _.splitToSeq(" -> ").map(_.splitToPair(",").map(_.toInt))
        .sliding(2).map(_.toPair)
        .flatMap { (a, b) =>
          val (x, y) = (a.first -> b.first) -> (a.second -> b.second)
          if a.first == b.first then (y.min to y.max).map(a.first -> _)
          else if a.second == b.second then (x.min to x.max).map(_ -> a.second)
          else Seq(a, b)
        }.toSeq.distinct
    }.distinct.map(_.swap)
    val sandPosition = sandStartPositionStr.splitToPair(",").map(_.toInt).swap
    val (rows, cols) = (sandPosition +: rockPositions).unzip
    val rowRange = rows.min to rows.max
    val colRange = cols.min to cols.max
    val positionToElements = (sandPosition -> Sand) +: rockPositions.map(_ -> Rock)
    val elementMap: Map[Position, Element] = Map.from(positionToElements)
    (sandPosition, elementMap, rowRange, colRange)

  private def simulate(sandPosition: Position, structure: Grid[Element], step: Position => Option[Position]): Int =
    var done = false
    while !done do
      var currPosition = sandPosition
      var canMoveSouth = true
      while !done && canMoveSouth do
        step(currPosition) match
          case Some(nextPosition) =>
            currPosition = nextPosition
          case None =>
            canMoveSouth = false
            structure(currPosition) = Sand
        done = !structure.withinRange(currPosition + Direction.SW.distance) || currPosition == sandPosition
    structure.count(_ == Sand)

  protected def part1Solution: Seq[String] => Unit = input =>
    val (sandPosition, elementMap, rowRange, colRange) = parse("500,0", input)
    val structure = new Grid(elementMap, Air, false, PositionRange(rowRange, colRange))
    val result = simulate(sandPosition, structure, stepPart1(structure)) - 1
    println(s"Result for part 1: $result")

  protected def part2Solution: Seq[String] => Unit = input =>
    val (sandPosition, elementMap, rowRange, _) = parse("500,0", input)
    val range = rowRange.start to (rowRange.end + 2)
    val structure = new Grid(elementMap, Air, false, PositionRange(range, InfRange))
    val result = simulate(sandPosition, structure, stepPart2(structure, range.end))
    println(s"Result for part 2: $result")
