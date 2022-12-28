package io.c6.aoc.util

import scala.annotation.tailrec
import scala.concurrent.duration.{Duration, NANOSECONDS}
import scala.collection.mutable

//noinspection ScalaWeakerAccess, ScalaUnusedSymbol
object Utils:
  def time[T](operation: => T): T =
    val startTime = System.nanoTime()
    val result = operation
    val elapsedDuration = Duration(System.nanoTime() - startTime, NANOSECONDS)
    println(s"Time taken: ${elapsedDuration.toMillis} milliseconds")
    result

  object SeqExt:
    extension[T] (seq: Seq[T])
      def split(separator: T): Seq[Seq[T]] =
        splitBy(_ == separator)

      def splitBy(predicate: T => Boolean): Seq[Seq[T]] =
        val parent = mutable.ListBuffer[Seq[T]]()
        val child = mutable.ListBuffer[T]()
        seq.foreach { value =>
          if predicate(value) then
            parent.append(child.toSeq)
            child.clear
          else
            child.append(value)
        }
        parent.append(child.toSeq)
        parent.toSeq

      def toPairOption: Option[(T, T)] =
        val pairSeq = seq.take(2)
        if pairSeq.length == 2 then Some(pairSeq.head -> pairSeq.last) else None
