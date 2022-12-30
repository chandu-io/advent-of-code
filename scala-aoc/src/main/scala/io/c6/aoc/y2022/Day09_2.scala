package io.c6.aoc.y2022

import io.c6.aoc.util.BaseSolution
import io.c6.aoc.util.Day.*
import io.c6.aoc.util.InputType.*
import io.c6.aoc.util.Year.*

import scala.annotation.targetName

@main def _2022_09_2: Unit = Day09_2()

//noinspection DuplicatedCode
object Day09_2 extends BaseSolution(_2022, _09):
  private val zero = Coordinate(0, 0)
  private val right = Distance(1, 0)
  private val left = Distance(-1, 0)
  private val up = Distance(0, 1)
  private val down = Distance(0, -1)
  private val none = Distance(0, 0)
  private val stringToDirection = Map("R" -> right, "L" -> left, "U" -> up, "D" -> down)

  private case class Coordinate(x: Int, y: Int) extends Product with Serializable:
    @targetName("moveBy")
    def +(distance: Distance): Coordinate = Coordinate(x + distance.dx, y + distance.dy)

    //noinspection ScalaWeakerAccess
    @targetName("distanceFrom")
    def -(other: Coordinate): Distance = Distance(x - other.x, y - other.y)

    @targetName("follow")
    def ~>(other: Coordinate): Coordinate =
      val distance = other - this
      if distance.toAbsolute > 1 then this + distance.clamped else this

    override def toString: String = s"($x, $y)"

  private case class Distance(dx: Int, dy: Int) extends Product with Serializable:
    def clamped: Distance =
      val clamp: (Int, Int, Int) => Int = (value, min, max) =>
        if value < min then min else if value > max then max else value
      Distance(clamp(dx, left.dx, right.dx), clamp(dy, down.dy, up.dy))

    def toAbsolute: Int = Math.max(Math.abs(dx), Math.abs(dy))

    override def toString: String = s"($dx, $dy)"

  private type Direction = Distance

  private class Instruction(val direction: Direction, val moves: Int)

  private def parse(str: String): Instruction = str.split(" ") match
    case Array(dirStr, movesStr) =>
      Instruction(stringToDirection.getOrElse(dirStr, none), movesStr.toIntOption.getOrElse(0))

  private def processInstructions(instructions: Seq[String], knotsCount: Int): Set[Coordinate] =
    if knotsCount < 2 then return Set(zero)
    val (_, uniqueTailCoordinates) = instructions.map(parse)
      .flatMap(instruction => Seq.fill(instruction.moves)(instruction.direction))
      .foldLeft(Seq.fill(knotsCount)(zero) -> Set(zero)) { case ((knots, visited), direction) =>
        val newKnots = knots.tail.scanLeft(knots.head + direction) { (h, t) => t ~> h }
        //printKnots(newKnots)
        newKnots -> visited.incl(newKnots.last)
      }
    uniqueTailCoordinates

  //noinspection ScalaUnusedSymbol
  private def printKnots(knots: Seq[Coordinate]): Unit =
    val minMaxRange: Seq[Int] => Range = seq => seq.reduce(Math.min) to seq.reduce(Math.max)
    val cols = minMaxRange(knots.map(_.x))
    val rows = minMaxRange(knots.map(_.y)).reverse
    rows.foreach { y =>
      cols.foreach { x =>
        val idx = knots.indexWhere(_ == Coordinate(x, y))
        val value = if idx == -1 then "." else if idx == 0 then "H" else idx.toString
        print(s" $value")
      }
      println()
    }
    println()

  protected def part1Solution: Seq[String] => Unit = input =>
    val result = processInstructions(input, 2).size
    println(s"Result for part 1: $result")

  protected def part2Solution: Seq[String] => Unit = input =>
    val result = processInstructions(input, 10).size
    println(s"Result for part 2: $result")
