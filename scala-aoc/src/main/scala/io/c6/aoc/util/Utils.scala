package io.c6.aoc.util

import scala.annotation.{tailrec, targetName}
import scala.collection.mutable
import scala.concurrent.duration.{Duration, NANOSECONDS}

//noinspection ScalaWeakerAccess, ScalaUnusedSymbol
object Utils:
  def time[T](operation: => T): T =
    val startTime = System.nanoTime()
    val result = operation
    val elapsedDuration = Duration(System.nanoTime() - startTime, NANOSECONDS)
    println(s"Time taken: ${elapsedDuration.toMillis} milliseconds")
    result

  object TripletImplicits:

    implicit final class DiamondAssoc0[A](private val self: A) extends AnyVal:
      @inline
      @targetName("toPair")
      def <>[B](y: B): (A, B) = (self, y)

    implicit final class DiamondAssoc1[A](private val self: A) extends AnyVal:
      @inline
      @targetName("toTriplet")
      def <>[B, C](y: (B, C)): (A, B, C) = (self, y._1, y._2)

    implicit final class DiamondAssoc2[A, B](private val self: (A, B)) extends AnyVal:
      @inline
      @targetName("toTriplet")
      def <>[C](y: C): (A, B, C) = (self._1, self._2, y)

  import TripletImplicits.*

  object StringExt:
    extension (str: String)
      def splitToSeq(regex: String, limit: Int = 0): Seq[String] = str.split(regex, limit).toSeq
      def splitToPair(regex: String, limit: Int = 0, fillValue: String = ""): (String, String) =
        str.split(regex, limit).take(2) match
          case Array(a, b) => a -> b
          case Array(a) => a -> fillValue
          case _ => fillValue -> fillValue
      def splitToPairOption(regex: String, limit: Int = 0): Option[(String, String)] =
        str.split(regex, limit).take(2) match
          case Array(a, b) => Some(a -> b)
          case _ => None
      def splitToTriplet(regex: String, limit: Int = 0, fillValue: String = ""): (String, String, String) =
        str.split(regex, limit).take(3) match
          case Array(a, b, c) => a <> b <> c
          case Array(a, b) => a <> b <> fillValue
          case Array(a) => a <> fillValue <> fillValue
          case _ => fillValue <> fillValue <> fillValue
      def splitToTripletOption(regex: String, limit: Int = 0): Option[(String, String, String)] =
        str.split(regex, limit).take(3) match
          case Array(a, b, c) => Some(a <> b <> c)
          case _ => None

  object IterableExt:
    extension[A] (iterable: Iterable[A])
      def split(separator: A): Iterable[Iterable[A]] =
        splitBy(_ == separator)

      def splitBy(predicate: A => Boolean): Iterable[Iterable[A]] =
        val parent = mutable.ListBuffer[Iterable[A]]()
        val child = mutable.ListBuffer[A]()
        iterable.foreach { value =>
          if predicate(value) then
            parent.append(child.toSeq)
            child.clear
          else
            child.append(value)
        }
        parent.append(child.toSeq)
        parent.toSeq

      def toPair: (A, A) =
        iterable.head -> iterable.tail.head

      /*def toPair(fillValue: A): (A, A) =
        val pairSeq = iterable.take(2)
        pairSeq.size match
          case 2 => pairSeq.head -> pairSeq.last
          case 1 => pairSeq.head -> fillValue
          case _ => fillValue -> fillValue*/

      def toPairOption: Option[(A, A)] =
        val pairSeq = iterable.take(2)
        pairSeq.size match
          case 2 => Some(pairSeq.head -> pairSeq.last)
          case _ => None

      def toTriplet: (A, A, A) =
        iterable.head <> iterable.tail.head <> iterable.tail.tail.head

      /*def toTriplet(fillValue: A): (A, A, A) =
        val tripletSeq = iterable.take(3)
        tripletSeq.size match
          case 3 => tripletSeq.head <> tripletSeq.tail.head <> tripletSeq.last
          case 2 => tripletSeq.head <> tripletSeq.last <> fillValue
          case 1 => tripletSeq.head <> fillValue <> fillValue
          case _ => fillValue <> fillValue <> fillValue*/

      def toTripletOption: Option[(A, A, A)] =
        val tripletSeq = iterable.take(3)
        tripletSeq.size match
          case 3 => Some(tripletSeq.head <> tripletSeq.tail.head <> tripletSeq.last)
          case _ => None

  object PairExt:
    extension[A, B] (pair: (A, B))
      def first: A = pair._1
      def second: B = pair._2
      def transform[A1, B1](fa: A => A1, fb: B => B1): (A1, B1) = fa(pair.first) -> fb(pair.second)

    extension[A] (pair: (A, A))
      def toSeq: Seq[A] = Seq(pair.first, pair.second)
      def map[A1](f: A => A1): (A1, A1) = pair.transform(f, f)
      def max(implicit ord: Ordering[A]): A = ord.max(pair.first, pair.second)
      def min(implicit ord: Ordering[A]): A = ord.min(pair.first, pair.second)
      def equiv(implicit ord: Ordering[A]): Boolean = ord.equiv(pair.first, pair.second)

    extension[A, B] (pair: (Option[A], Option[B]))
      def flatten: Option[(A, B)] = (pair.first, pair.second) match
        case Some(a) -> Some(b) => Some(a -> b)
        case _ => None

  object TripletExt:
    extension[A, B, C] (triplet: (A, B, C))
      def first: A = triplet._1
      def second: B = triplet._2
      def third: C = triplet._3
      def transform[A1, B1, C1](fa: A => A1, fb: B => B1, fc: C => C1): (A1, B1, C1) =
        fa(triplet.first) <> fb(triplet.second) <> fc(triplet.third)

    extension[A] (triplet: (A, A, A))
      def toSeq: Seq[A] = Seq(triplet.first, triplet.second, triplet.third)
      def map[B](f: A => B): (B, B, B) = triplet.transform(f, f, f)
      def max(implicit ord: Ordering[A]): A =
        ord.max(ord.max(triplet.first, triplet.second), ord.max(triplet.second, triplet.third))
      def min(implicit ord: Ordering[A]): A =
        ord.min(ord.min(triplet.first, triplet.second), ord.min(triplet.second, triplet.third))
      def equiv(implicit ord: Ordering[A]): Boolean =
        ord.equiv(triplet.first, triplet.second) && ord.equiv(triplet.second, triplet.third)

    extension[A, B, C] (triplet: (Option[A], Option[B], Option[C]))
      def flatten: Option[(A, B, C)] = triplet.first -> triplet.second -> triplet.third match
        case Some(a) -> Some(b) -> Some(c) => Some(a <> b <> c)
        case _ => None
