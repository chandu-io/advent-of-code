package io.c6.aoc.y2022

import io.c6.aoc.BaseSolution
import io.c6.aoc.Day.*
import io.c6.aoc.InputType.*
import io.c6.aoc.Year.*

import scala.collection.mutable

//noinspection DuplicatedCode
object Day05 extends BaseSolution:
  override protected def part1InputFileName: String = BaseSolution.getInputFileName(_2022, _05, A1)

  private type Stack = mutable.Stack[Char]

  extension (stack: Stack)
    private def load(elem: Char): Stack = if (elem == ' ') stack else stack.append(elem)

  private def nothing: Unit = ()
  private val gap = 4

  private def loadStacks(stacks: Array[Stack], instruction: String): Unit =
    val len = instruction.length
    var m = 0
    while (m < len)
      stacks(m / gap).load(instruction(m + 1))
      m += gap

  private def rearrangeStacks(stacks: Array[Stack], instruction: String): Unit =
    val (steps, from, to) = instruction.split(' ') match
      case Array(_, a, _, b, _, c) => (a.toInt, b.toInt - 1, c.toInt - 1)
    val creates = (1 to steps).map { _ => stacks(from).pop() }
    stacks(to).pushAll(creates)

  private def rearrangeStacksPart2(stacks: Array[Stack], instruction: String): Unit =
    val (steps, from, to) = instruction.split(' ') match
      case Array(_, a, _, b, _, c) => (a.toInt, b.toInt - 1, c.toInt - 1)
    val creates = (1 to steps).map { _ => stacks(from).pop() }
    stacks(to).pushAll(creates.reverse)

  override protected def part1Solution: Seq[String] => Unit = { input =>
    val len = input.head.length
    val stacks = Array.fill((len + 1) / gap)(new Stack)
    input.foreach { line =>
      val trimmedLine = line.trim
      if (trimmedLine.startsWith("["))
        loadStacks(stacks, line)
      else if (trimmedLine.startsWith("move"))
        rearrangeStacks(stacks, line)
      else
        nothing
    }
//    stacks.foreach(println)
    val result = stacks.map(_.top).mkString
    println(s"Result for part 1: $result")
  }

  override protected def part2Solution: Seq[String] => Unit = { input =>
    val len = input.head.length
    val stacks = Array.fill((len + 1) / gap)(new Stack)
    input.foreach { line =>
      val trimmedLine = line.trim
      if (trimmedLine.startsWith("["))
        loadStacks(stacks, line)
      else if (trimmedLine.startsWith("move"))
        rearrangeStacksPart2(stacks, line)
      else
        nothing
    }
//    stacks.foreach(println)
    val result = stacks.map(_.top).mkString
    println(s"Result for part 2: $result")
  }

@main def runDay05: Unit = Day05.run
