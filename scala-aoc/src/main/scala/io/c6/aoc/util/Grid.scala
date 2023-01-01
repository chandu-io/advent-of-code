package io.c6.aoc.util

import io.c6.aoc.util.Utils.PairExt.*

import scala.collection.mutable
import scala.util.Try
import Grid.*

// Grid(map, rowRange = -Inf to Inf, colRange = -Inf to Inf)

//noinspection ScalaWeakerAccess,ScalaUnusedSymbol
class Grid[A](
               dataMap: Map[Position, A],
               defaultCellData: A,
               readOnly: Boolean = true,
               positionRange: PositionRange = Grid.DefaultPositionRange,
             ):
  private val rowRange = positionRange.rowRange
  private val colRange = positionRange.colRange

  private val cells = mutable.HashMap[Position, Cell]()
  dataMap.foreach { (position, data) =>
    if withinRange(position) then cells.put(position, Cell(position, data))
  }

  def apply(row: Int, col: Int): Cell = apply(row -> col)

  def apply(position: Position): Cell =
    require(withinRange(position), s"Invalid cell position: $position")
    if cells.contains(position) then
      cells(position)
    else
      val cell = Cell(position, defaultCellData)
      cells.put(position, cell)
      cell

  def update(row: Int, col: Int, value: A): Unit = update(row -> col, value)

  def update(position: Position, value: A): Unit =
    require(!readOnly, "Unsupported operation on read-only grid")
    require(withinRange(position), s"Invalid cell position: $position")
    cells.put(position, Cell(position, value))

  def find(predicate: A => Boolean): Option[Cell] =
    cells.find((_, cell) => predicate(cell.data)).map(_.second)

  def exists(predicate: A => Boolean): Boolean =
    cells.exists((_, cell) => predicate(cell.data))

  def count(predicate: A => Boolean): Int =
    cells.count((_, cell) => predicate(cell.data))

  def withinRange(row: Int, col: Int): Boolean = (rowRange contains row) && (colRange contains col)

  def withinRange(position: Position): Boolean = withinRange(position.first, position.second)

  def print(toString: A => String = _.toString): Unit =
    // TODO: Use trimmed ranges to print
    rowRange.foreach { row =>
      colRange.foreach { col =>
        val s = toString(Try(cells(row -> col)).map(_.data).getOrElse(defaultCellData))
        Console.print(s" $s")
      }
      println
    }
    println

  case class Cell(position: Position, data: A):
    require(withinRange(position), s"Invalid cell position: $position")

    private def getNeighbour(direction: Direction): Cell =
      val newPosition = position + direction.distance
      if withinRange(newPosition) then apply(newPosition) else this

    lazy val N: Cell = getNeighbour(Direction.N)
    lazy val NE: Cell = getNeighbour(Direction.NE)
    lazy val E: Cell = getNeighbour(Direction.E)
    lazy val SE: Cell = getNeighbour(Direction.SE)
    lazy val S: Cell = getNeighbour(Direction.S)
    lazy val SW: Cell = getNeighbour(Direction.SW)
    lazy val W: Cell = getNeighbour(Direction.W)
    lazy val NW: Cell = getNeighbour(Direction.NW)

    lazy val adjacentCells: Seq[Cell] = Seq(N, E, S, W).filter(_ != this)

    lazy val diagonalCells: Seq[Cell] = Seq(NE, SE, SW, NW).filter(_ != this)

    lazy val surroundingCells: Seq[Cell] = adjacentCells ++ diagonalCells

    def neighbours(directions: Seq[Direction]): Seq[Cell] = directions.map {
      _ match
        case Direction.N => N
        case Direction.NE => NE
        case Direction.E => E
        case Direction.SE => SE
        case Direction.S => S
        case Direction.SW => SW
        case Direction.W => W
        case Direction.NW => NW
    }.filter(_ != this)

//noinspection ScalaWeakerAccess
object Grid:
  type Position = (Int, Int)

  case class PositionRange(rowRange: Range, colRange: Range)

  val InfRange: Range = Int.MinValue to Int.MaxValue
  val DefaultPositionRange: PositionRange =
    PositionRange(InfRange, InfRange)
