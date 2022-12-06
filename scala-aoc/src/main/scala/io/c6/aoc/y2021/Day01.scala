package io.c6.aoc.y2021

import scala.io.{BufferedSource, Source}

object Day01:

  //private val input: BufferedSource = Source.fromResource("input/2021-day-01-sample-input.txt")
  private val input: BufferedSource = Source.fromResource("input/2021-day-01-input.txt")

  private def countDepthIncrements(depths: Seq[Int]): Int =
    depths.sliding(2).count {
      case a :: b :: Nil => b > a
      case _ => false
    }

  @main def solutionY2021Day01: Unit =
    val depths = input.getLines().map(_.toInt).toSeq
    val result1 = countDepthIncrements(depths)
    println(result1)

    val depthSums = depths.sliding(3).map(_.sum).toSeq
    val result2 = countDepthIncrements(depthSums)
    println(result2)

    input.close
