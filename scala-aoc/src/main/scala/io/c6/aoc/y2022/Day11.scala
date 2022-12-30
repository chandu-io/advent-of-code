package io.c6.aoc.y2022

import io.c6.aoc.util.BaseSolution
import io.c6.aoc.util.Day.*
import io.c6.aoc.util.InputType.*
import io.c6.aoc.util.Year.*
import io.c6.aoc.util.Utils.PairExt.*

import scala.collection.mutable
import scala.language.implicitConversions

@main def _2022_11: Unit = Day11(Actual)

object Day11 extends BaseSolution(_2022, _11):
  private type Operation = Function[Long, Long]
  private val noop: Operation = identity[Long]

  private sealed trait Operator

  private object Plus extends Operator

  private object Times extends Operator

  private object Identity extends Operator

  private given Conversion[String, Operator] = _ match
    case "+" => Plus
    case "*" => Times
    case _ => Identity

  private def composeOperation(operator: Operator, maybeOperand: Option[Long]): Operation =
    (operator, maybeOperand) match
      //@formatter:off
      case Plus -> Some(operand) => x => x + operand
      case Times -> Some(operand) => x => x * operand
      case Plus -> None => x => x + x
      case Times -> None => x => x * x
      case Identity -> _ => noop
      //@formatter:on

  private class Monkey(private val notes: Seq[String]):
    private val items =
      val _items = notes(1).substring(18).split(", ").map(_.toLong)
      mutable.Queue.from(_items)
    private val operation: Operation =
      val old = "old"
      notes(2).substring(19).split(" ") match
        case Array(left, middle, right) =>
          if left == old && right == old then composeOperation(middle, None)
          else if left == old then composeOperation(middle, Some(right.toLong))
          else if right == old then composeOperation(middle, Some(left.toLong))
          else noop
        case _ => noop
    val testDivisor: Long = notes(3).substring(21).toLong
    private val successMonkeyId = notes(4).substring(29).toInt
    private val failureMonkeyId = notes(5).substring(30).toInt
    private var maybeSuccessMonkey: Option[Monkey] = None
    private var maybeFailureMonkey: Option[Monkey] = None
    private var inspectionCount = 0L

    def updateTestMonkeys(all: Seq[Monkey]): Unit =
      val zipped = all.zipWithIndex
      maybeSuccessMonkey = zipped.find(_.second == successMonkeyId).map(_.first)
      maybeFailureMonkey = zipped.find(_.second == failureMonkeyId).map(_.first)

    private def receiveItem(item: Long): Unit = items.enqueue(item)

    def inspectItems(worryReducer: Operation): Unit =
      while items.nonEmpty do
        (maybeSuccessMonkey, maybeFailureMonkey).flatten match
          case Some(successMonkey -> failureMonkey) =>
            inspectionCount += 1
            var item = items.dequeue()
            item = operation(item)
            item = worryReducer(item)
            if item % testDivisor == 0 then successMonkey.receiveItem(item)
            else failureMonkey.receiveItem(item)
          case None => Console.err.println("Before inspecting, call updateTestMonkeys method")

    def inspections: Long = inspectionCount

  private def parse(input: Seq[String]): Seq[Monkey] =
    val monkeys = input.grouped(7).map(_.filter(_.nonEmpty)).map(Monkey(_)).toSeq
    monkeys.foreach(_.updateTestMonkeys(monkeys))
    monkeys

  private def monkeyBusiness(monkeys: Seq[Monkey], rounds: Int, worryReducer: Operation): Long =
    (1 to rounds).foreach { _ => monkeys.foreach(_.inspectItems(worryReducer)) }
    monkeys.sortBy(_.inspections).takeRight(2).map(_.inspections).product

  protected def part1Solution: Seq[String] => Unit = input =>
    val monkeys = parse(input)
    val worryReducer: Operation = _ / 3
    val result = monkeyBusiness(monkeys, 20, worryReducer)
    println(s"Result for part 1: $result")

  protected def part2Solution: Seq[String] => Unit = input =>
    val monkeys = parse(input)
    val worryReducer: Operation = _ % monkeys.map(_.testDivisor).product
    val result = monkeyBusiness(monkeys, 10_000, worryReducer)
    println(s"Result for part 2: $result")
