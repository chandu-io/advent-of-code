package io.c6.aoc.y2022

import io.c6.aoc.BaseSolution
import io.c6.aoc.BaseSolution.*
import io.c6.aoc.Day.*
import io.c6.aoc.InputType.*
import io.c6.aoc.Year.*

import scala.collection.mutable

object Day05 extends BaseSolution:
  override protected def part1InputFileName: String = getInputFileName(_2022, _05, A1)

  private type Crate = Char
  private type Stack = mutable.Stack[Crate]

  private enum Crane:
    case CrateMover9000, CrateMover9001

  private enum Instruction:
    case LoadInstruction, MoveInstruction, SomethingElse

  import Crane.*
  import Instruction.*

  extension (stack: Stack)
    private def load(crate: Crate): Stack = if (crate.isUpper) stack.append(crate) else stack

  extension (crane: Crane)
    private def move(count: Int, from: Stack, to: Stack): Unit = crane match
      case CrateMover9000 => (1 to count).foreach(_ => to.push(from.pop()))
      case CrateMover9001 => to.pushAll((1 to count).map(_ => from.pop()).reverse)

  // distance between crates in the load instruction
  private val gap = 4

  private def parseMoveInstruction(line: String): (Int, Int, Int) =
    line.split(' ') match
      case Array(_, a, _, b, _, c) => (a.toInt, b.toInt - 1, c.toInt - 1)

  private def parseLoadInstruction(line: String): Seq[(Int, Crate)] =
    (0 until line.length by gap).map { i => (i / gap) -> line(i + 1) }

  private def toInstruction(line: String): Instruction =
    Option(line).map(_.trim).filter(_.nonEmpty).getOrElse(" ").charAt(0) match
      case '[' => LoadInstruction
      case 'm' => MoveInstruction
      case _ => SomethingElse

  private def processInstruction(stacks: Array[Stack], crane: Crane)(line: String): Unit =
    toInstruction(line) match
      case LoadInstruction =>
        parseLoadInstruction(line).foreach { case id -> crate =>
          stacks(id).load(crate)
        }
      case MoveInstruction =>
        val (count, from, to) = parseMoveInstruction(line)
        crane.move(count, stacks(from), stacks(to))
      case SomethingElse => ()

  override protected def part1Solution: Seq[String] => Unit = { input =>
    val stacks = Array.fill((input.head.length + 1) / gap)(new Stack)
    input.foreach(processInstruction(stacks, CrateMover9000))
    val result = stacks.map(_.top).mkString
    println(s"Result for part 1: $result")
  }

  override protected def part2Solution: Seq[String] => Unit = { input =>
    val stacks = Array.fill((input.head.length + 1) / gap)(new Stack)
    input.foreach(processInstruction(stacks, CrateMover9001))
    val result = stacks.map(_.top).mkString
    println(s"Result for part 2: $result")
  }

@main def runDay05: Unit = Day05.run
