package io.c6.aoc.y2022

import io.c6.aoc.util.BaseSolution
import io.c6.aoc.util.Day.*
import io.c6.aoc.util.InputType.*
import io.c6.aoc.util.Year.*

import scala.collection.mutable

@main def _2022_05: Unit = Day05()

object Day05 extends BaseSolution(_2022, _05):
  private type Crate = Char
  private type Stack = mutable.Stack[Crate]

  private sealed trait Crane

  private object CrateMover9000 extends Crane

  private object CrateMover9001 extends Crane

  private sealed trait Instruction

  private object LoadInstruction extends Instruction

  private object MoveInstruction extends Instruction

  private object SomethingElse extends Instruction

  extension (stack: Stack)
    private def load(crate: Crate): Stack = if crate.isUpper then stack.append(crate) else stack

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

  private def processInstruction(crane: Crane)(stacks: Array[Stack], line: String): Array[Stack] =
    toInstruction(line) match
      case LoadInstruction =>
        parseLoadInstruction(line).foreach { case id -> crate => stacks(id).load(crate) }
      case MoveInstruction =>
        val (count, from, to) = parseMoveInstruction(line)
        crane.move(count, stacks(from), stacks(to))
      case SomethingElse => ()
    stacks

  override protected def part1Solution: Seq[String] => Unit = input =>
    val stacks = Array.fill((input.head.length + 1) / gap)(new Stack)
    val result = input.foldLeft(stacks)(processInstruction(CrateMover9000)).map(_.top).mkString
    println(s"Result for part 1: $result")

  override protected def part2Solution: Seq[String] => Unit = input =>
    val stacks = Array.fill((input.head.length + 1) / gap)(new Stack)
    val result = input.foldLeft(stacks)(processInstruction(CrateMover9001)).map(_.top).mkString
    println(s"Result for part 2: $result")
