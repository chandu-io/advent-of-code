package io.c6.aoc.y2022

import io.c6.aoc.util.BaseSolution
import io.c6.aoc.util.BaseSolution.*
import io.c6.aoc.util.Day.*
import io.c6.aoc.util.InputType.*
import io.c6.aoc.util.Year.*

//noinspection DuplicatedCode
object Day08 extends BaseSolution:
  override protected def part1InputFileName: String = getInputFileName(_2022, _08, A1)

  private type Grid[T] = Array[Array[T]]

  extension[T] (grid: Grid[T])
    private def print(toString: Function[T, String]): Unit =
      grid.map(_.map(toString).mkString(" ")).foreach(println)
      println()

  private def isVisibleFromOutside(adjacentHeights: Seq[Byte])(height: Byte): Boolean =
    adjacentHeights.exists(_ >= height)

  private def visibleTreeCount(adjacentHeights: Seq[Byte])(height: Byte): Int =
    val idx = adjacentHeights.indexWhere(_ >= height)
    if idx == -1 then adjacentHeights.length
    else if adjacentHeights(idx) == height then idx + 1
    else idx

  override protected def part1Solution: Seq[String] => Unit = input =>
    val rows -> cols = input.length -> input.head.length
    val treeGrid: Grid[Byte] = Array.ofDim[Byte](rows, cols)
    val visibilityGrid: Grid[Boolean] = Array.ofDim[Boolean](rows, cols)

    for i <- 0 until rows; j <- 0 until cols do
      treeGrid(i)(j) = input(i)(j).asDigit.toByte
      visibilityGrid(i)(j) = false
    //treeGrid.print(_.toString)

    for i <- 0 until rows; j <- 0 until cols do (i, j) match
      case (a, _) if a == 0 || a == rows - 1 =>
        visibilityGrid(i)(j) = true
      case (_, b) if b == 0 || b == cols - 1 =>
        visibilityGrid(i)(j) = true
      case (a, b) =>
        val h = treeGrid(i)(j)
        val left = isVisibleFromOutside((0 until b).map(treeGrid(a)(_)))(h)
        val right = isVisibleFromOutside((b + 1 until cols).map(treeGrid(a)(_)))(h)
        val up = isVisibleFromOutside((0 until a).map(treeGrid(_)(b)))(h)
        val down = isVisibleFromOutside((a + 1 until rows).map(treeGrid(_)(b)))(h)
        visibilityGrid(i)(j) = !(left && right && up && down)
    //visibilityGrid.print(if _ then "1" else "0")

    val result = visibilityGrid.map(_.count(identity)).sum
    println(s"Result for part 1: $result")

  override protected def part2Solution: Seq[String] => Unit = input =>
    val rows -> cols = input.length -> input.head.length
    val treeGrid: Grid[Byte] = Array.ofDim[Byte](rows, cols)
    val scenicScoreGrid: Grid[Int] = Array.ofDim[Int](rows, cols)

    for i <- 0 until rows; j <- 0 until cols do
      treeGrid(i)(j) = input(i)(j).asDigit.toByte
      scenicScoreGrid(i)(j) = 0

    for i <- 0 until rows; j <- 0 until cols do (i, j) match
      case (a, _) if a == 0 || a == rows - 1 =>
        scenicScoreGrid(i)(j) = 0
      case (_, b) if b == 0 || b == cols - 1 =>
        scenicScoreGrid(i)(j) = 0
      case (a, b) =>
        val h = treeGrid(i)(j)
        val left = visibleTreeCount((0 until b).reverse.map(treeGrid(a)(_)))(h)
        val right = visibleTreeCount((b + 1 until cols).map(treeGrid(a)(_)))(h)
        val up = visibleTreeCount((0 until a).reverse.map(treeGrid(_)(b)))(h)
        val down = visibleTreeCount((a + 1 until rows).map(treeGrid(_)(b)))(h)
        scenicScoreGrid(i)(j) = left * right * up * down
    //scenicScoreGrid.print(s => s"%2d".format(s))

    val result = scenicScoreGrid.map(_.max).max
    println(s"Result for part 2: $result")

@main def runDay08: Unit = Day08.run
