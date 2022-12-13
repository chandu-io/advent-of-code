package io.c6.aoc.y2021

import io.c6.aoc.BaseSolution
import io.c6.aoc.Day._01
import io.c6.aoc.Year._2021

import scala.io.{BufferedSource, Source}

private object Day01 extends BaseSolution:

  protected val inputFileName: String = BaseSolution.getInputFileName(_2021, _01)

  private def countDepthIncrements(depths: Seq[Int]): Int =
    depths.sliding(2).count {
      case a :: b :: Nil => b > a
      case _ => false
    }

  protected override def solution: Seq[String] => Unit = { input =>
    val depths = input.map(_.toInt)
    val result1 = countDepthIncrements(depths)
    println(result1)

    val depthSums = depths.sliding(3).map(_.sum).toSeq
    val result2 = countDepthIncrements(depthSums)
    println(result2)
  }

@main def runDay01: Unit = Day01.run
