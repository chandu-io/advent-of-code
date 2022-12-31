package io.c6.aoc.y2022

import io.c6.aoc.util.BaseSolution
import io.c6.aoc.util.Day.*
import io.c6.aoc.util.InputType.*
import io.c6.aoc.util.Year.*

import scala.collection.mutable
import scala.util.Try

@main def _2022_12: Unit = Day12()

object Day12 extends BaseSolution(_2022, _12):
  private type Location = (Int, Int)
  private type Elevation = Int

  // TODO: Refactor using util.Grid class
  private class Grid private(rows: Int, cols: Int, start: Location, end: Location, cellData: Array[Array[Elevation]]):
    require(rows > 0, s"Invalid row count: $rows")
    require(cols > 0, s"Invalid column count: $cols")
    require(locations contains start, s"Invalid start location: $start")
    require(locations contains end, s"Invalid end location: $end")
    require(cellData.length == rows && cellData.head.length == cols, "Invalid cell data")

    private lazy val locations: Set[Location] =
      (for x <- 0 until rows; y <- 0 until cols yield x -> y).toSet

    private lazy val getCell: Location => Cell =
      val cells = Array.ofDim[Cell](rows, cols)
      locations.foreach { (x, y) => cells(x)(y) = new Cell(x -> y, cellData(x)(y)) }
      (x, y) => cells(x)(y)

    private lazy val startCell: Cell = getCell(start)

    private lazy val endCell: Cell = getCell(end)

    def shortestDistance(isComplete: Cell => Boolean): Int =
      val q = mutable.Queue[(Cell, Int)](endCell -> 0)
      var distance = -1
      while q.nonEmpty do
        val (cell, steps) = q.dequeue
        if cell.isNotVisited then
          cell.markAsVisited
          if isComplete(cell) then
            distance = steps
            q.clear
          else
            q.enqueueAll(cell.nextAdjacentCells.map(_ -> (steps + 1)))
      distance

    //noinspection ScalaUnusedSymbol
    def printGrid: Unit =
      (0 until rows).foreach { x =>
        (0 until cols).foreach { y =>
          val location = x -> y
          val cellValue =
            if location == start then 'S' else if location == end then 'E'
            else getCell(location).elevation.toChar
          print(s" $cellValue")
        }
        println
      }
      println

    //noinspection ScalaUnusedSymbol
    def printVisibilityGrid: Unit =
      (0 until rows).foreach { x =>
        (0 until cols).foreach { y => print(s" ${if getCell(x -> y).isNotVisited then '0' else '1'}") }
        println
      }
      println

    class Cell(val location: Location, val elevation: Elevation):
      require(locations contains location, s"Invalid cell location: $location")
      private var visited = false
      private lazy val adjacentCells: Seq[Cell] = location match
        case x -> y =>
          Set((x - 1) -> y, (x + 1) -> y, x -> (y - 1), x -> (y + 1)).intersect(locations).toSeq.map(getCell)

      def isStartCell: Boolean = startCell.location == location

      def isLowestElevatedCell: Boolean = startCell.elevation == elevation

      def isNotVisited: Boolean = !visited

      def markAsVisited: Unit = visited = true

      def nextAdjacentCells: Seq[Cell] =
        adjacentCells.filter(_.isNotVisited).filter(elevation - _.elevation <= 1)

  private object Grid:
    def apply(input: Seq[String]): Grid =
      val heightmap = input.map(_.toCharArray.toSeq)
      val rows = input.length
      val cols = if rows == 0 then 0 else input.head.length
      var start = 0 -> 0
      var end = 0 -> 0
      val cellData = Array.ofDim[Elevation](rows, cols)
      for x <- 0 until rows; y <- 0 until cols do
        val location = x -> y
        val char = Try(heightmap(x)(y)).getOrElse('a')
        if char == 'S' then start = location
        if char == 'E' then end = location
        cellData(x)(y) = elevation(char)
      new Grid(rows, cols, start, end, cellData)

    private def elevation(c: Char): Int = if c == 'S' then 'a' else if c == 'E' then 'z' else c

  protected def part1Solution: Seq[String] => Unit = input =>
    val grid = Grid(input)
    val result = grid.shortestDistance(_.isStartCell)
    println(s"Result for part 1: $result")

  protected def part2Solution: Seq[String] => Unit = input =>
    val grid = Grid(input)
    val result = grid.shortestDistance(_.isLowestElevatedCell)
    println(s"Result for part 2: $result")
