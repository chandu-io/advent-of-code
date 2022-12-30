package io.c6.aoc.y2022

import io.c6.aoc.util.BaseSolution
import io.c6.aoc.util.Day.*
import io.c6.aoc.util.InputType.*
import io.c6.aoc.util.Year.*

import scala.collection.mutable
import scala.language.implicitConversions

@main def _2022_09: Unit = Day09()

//noinspection DuplicatedCode
object Day09 extends BaseSolution(_2022, _09):
  private type Position = (Int, Int)

  private sealed trait Direction(val dx: Int, val dy: Int)

  private object Right extends Direction(1, 0)

  private object Left extends Direction(-1, 0)

  private object Up extends Direction(0, 1)

  private object Down extends Direction(0, -1)

  private type Instruction = (Direction, Int)

  extension (a: Position)
    private def x: Int = a._1
    private def y: Int = a._2
    private def moveTowards(direction: Direction): Position = (a.x + direction.dx, a.y + direction.dy)
    private def isTowardsRight(b: Position): Boolean = b.x > a.x
    private def isTowardsLeft(b: Position): Boolean = b.x < a.x
    private def isTowardsUp(b: Position): Boolean = b.y > a.y
    private def isTowardsDown(b: Position): Boolean = b.y < a.y
    private def distanceFrom(b: Position): Int =
      val (x, y) = (a.x - b.x) -> (a.y - b.y)
      Math.sqrt(x * x + y * y).toInt
    private def follow(b: Position): Position =
      if b.distanceFrom(a) > 1 then
        var a1 = a
        a1 = if a1.isTowardsRight(b) then a1.moveTowards(Right) else a1
        a1 = if a1.isTowardsLeft(b) then a1.moveTowards(Left) else a1
        a1 = if a1.isTowardsUp(b) then a1.moveTowards(Up) else a1
        a1 = if a1.isTowardsDown(b) then a1.moveTowards(Down) else a1
        a1
      else a

  private given Conversion[String, Instruction] = _.split(" ") match
    case Array(dirStr, movesStr) =>
      val direction = dirStr match
        case "R" => Right
        case "L" => Left
        case "U" => Up
        case _ => Down
      direction -> movesStr.toInt

  private class MotionTracker(val tailSize: Int):
    private var head: Position = 0 -> 0
    private var tail: List[Position] = List.fill(tailSize)(head)
    private val visited: mutable.Set[Position] = mutable.HashSet(head)

    private def moveTowards(direction: Direction): Unit =
      val knots = tail.scanLeft(head.moveTowards(direction)) { (h, t) => t.follow(h) }
      head = knots.head
      tail = knots.tail
      visited += tail.last
      //printKnots(head :: tail)

    def move(instruction: Instruction): Unit = instruction match
      case direction -> moves => (1 to moves).foreach(_ => moveTowards(direction))

    def uniqueVisitedPositions: Int = visited.size

  //noinspection ScalaUnusedSymbol
  private def printKnots(knotPositions: List[Position]): Unit =
    val minMaxRange: List[Int] => Range = list => list.reduce(Math.min) to list.reduce(Math.max)
    val cols = minMaxRange(knotPositions.map(_.x))
    val rows = minMaxRange(knotPositions.map(_.y)).reverse
    rows.foreach { y =>
      cols.foreach { x =>
        val idx = knotPositions.indexWhere(_ == x -> y)
        val value = if idx == -1 then "." else if idx == 0 then "H" else idx.toString
        print(s" $value")
      }
      println()
    }
    println()

  private def visitedPositions(input: Seq[String], knots: Int): Int =
    val motionTracker = new MotionTracker(knots - 1)
    input.foreach(motionTracker.move(_))
    motionTracker.uniqueVisitedPositions

  protected def part1Solution: Seq[String] => Unit = input =>
    val result = visitedPositions(input, 2)
    println(s"Result for part 1: $result")

  protected def part2Solution: Seq[String] => Unit = input =>
    val result = visitedPositions(input, 10)
    println(s"Result for part 2: $result")
