package io.c6.aoc.y2022

import io.c6.aoc.util.BaseSolution
import io.c6.aoc.util.Day.*
import io.c6.aoc.util.InputType.*
import io.c6.aoc.util.Year.*

import scala.collection.mutable

@main def _2022_13: Unit = Day13()

object Day13 extends BaseSolution(_2022, _13):

  private val start = "["
  private val end = "]"
  private val separator = ","
  private val empty = ""
  private val space = " "
  private type Signal = Packet | Data

  private case class Packet(signals: Signal*) extends Ordered[Packet]:
    private def appended(signal: Signal): Packet = Packet(signals :+ signal: _*)

    def compare(other: Packet): Int =
      val result = signals.zip(other.signals).map {
        case (Data(x), Data(y)) => x.compare(y)
        case (packet: Packet, data: Data) => packet.compare(Packet(data))
        case (data: Data, packet: Packet) => Packet(data).compare(packet)
        case (packetLeft: Packet, packetRight: Packet) => packetLeft.compare(packetRight)
        case _ => 0
      }
      val (m, n) = signals.length -> other.signals.length
      result.dropWhile(_ == 0).headOption.getOrElse(m.compare(n))

    override def toString: String = signals.mkString(start, separator, end)

  private case class Data(value: Int):
    override def toString: String = value.toString

  private object Packet:
    def apply(str: String): Packet =
      if str.isEmpty || str.head != start.head || str.last != end.head then return Packet()
      val stack = mutable.Stack[Packet]()
      var num = empty
      var packet = Packet()
      str.replace(space, empty).foreach { c =>
        if c == start.head then
          stack.push(Packet())
        else if c.isDigit then
          num += c
        else if c == end.head then
          var popped = stack.pop
          if num.nonEmpty then
            popped = popped.appended(Data(num.toInt))
            num = empty
          if stack.isEmpty then
            packet = popped
          else
            stack.push(stack.pop.appended(popped))
        else if c == separator.head then
          if num.nonEmpty then
            stack.push(stack.pop.appended(Data(num.toInt)))
            num = empty
      }
      packet

  protected def part1Solution: Seq[String] => Unit = input =>
    import io.c6.aoc.util.Utils.SeqExt.*
    val packetPairs = input.split(empty)
    val result = packetPairs.zipWithIndex
      .filter { (seq, _) => seq.map(Packet(_)).toPairOption.exists(_ < _) }
      .map((_, idx) => idx + 1).sum
    println(s"Result for part 1: $result")

  protected def part2Solution: Seq[String] => Unit = input =>
    implicit val ordering: Ordering[String] = Packet(_) compare Packet(_)
    val dividers = Seq("[[2]]", "[[6]]")
    val unorderedPackets = input.filter(_.nonEmpty) ++ dividers
    val result = unorderedPackets.sorted.zipWithIndex
      .filter { (p, _) => dividers.contains(p) }
      .map((_, idx) => idx + 1).product
    println(s"Result for part 2: $result")
