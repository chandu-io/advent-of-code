package io.c6.aoc.util

sealed trait Direction(val dx: Int, val dy: Int):
  def distance: (Int, Int) = dx -> dy

//noinspection ScalaUnusedSymbol,ScalaWeakerAccess
object Direction:

  object N extends Direction(-1, 0):
    override def toString: String = "N"

  object NE extends Direction(-1, 1):
    override def toString: String = "NE"

  object E extends Direction(0, 1):
    override def toString: String = "E"

  object SE extends Direction(1, 1):
    override def toString: String = "SE"

  object S extends Direction(1, 0):
    override def toString: String = "S"

  object SW extends Direction(1, -1):
    override def toString: String = "SW"

  object W extends Direction(0, -1):
    override def toString: String = "W"

  object NW extends Direction(-1, -1):
    override def toString: String = "NW"

  val cardinals: Seq[Direction] = Seq(N, E, S, W)
  val interCardinals: Seq[Direction] = Seq(NE, SE, SW, NW)
  val all: Seq[Direction] = cardinals ++ interCardinals
  val northern: Seq[Direction] = Seq(N, NW, NE)
  val eastern: Seq[Direction] = Seq(E, NE, SE)
  val southern: Seq[Direction] = Seq(S, SW, SE)
  val western: Seq[Direction] = Seq(W, NW, SW)
