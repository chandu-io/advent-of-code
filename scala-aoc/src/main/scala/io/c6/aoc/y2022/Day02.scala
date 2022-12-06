package io.c6.aoc.y2022

import scala.io.{BufferedSource, Source}

object Day02:

  //private val input: BufferedSource = Source.fromResource("input/2022-day-02-sample-input.txt")
  private val input: BufferedSource = Source.fromResource("input/2022-day-02-input.txt")

  private enum Outcome(val points: Int):
    case Win extends Outcome(6)
    case Draw extends Outcome(3)
    case Loose extends Outcome(0)

  private enum Shape(val points: Int):
    case Rock extends Shape(1)
    case Paper extends Shape(2)
    case Scissors extends Shape(3)

  private enum OpponentPlay(val shape: Shape):
    case A extends OpponentPlay(Rock)
    case B extends OpponentPlay(Paper)
    case C extends OpponentPlay(Scissors)

  private enum MyResponse(val shape: Shape):
    case X extends MyResponse(Rock)
    case Y extends MyResponse(Paper)
    case Z extends MyResponse(Scissors)

  import Outcome.*
  import Shape.*
  import OpponentPlay.*
  import MyResponse.*

  private object OpponentPlay:
    def from(str: String): OpponentPlay = str match
      case "A" => A
      case "B" => B
      case _ => C

  private object MyResponse:
    def from(str: String): MyResponse = str match
      case "X" => X
      case "Y" => Y
      case _ => Z

  private val outcomes: Map[(Shape, Shape), Outcome] = Map(
    (Rock, Rock) -> Draw,
    (Rock, Paper) -> Win,
    (Rock, Scissors) -> Loose,
    (Paper, Rock) -> Loose,
    (Paper, Paper) -> Draw,
    (Paper, Scissors) -> Win,
    (Scissors, Rock) -> Win,
    (Scissors, Paper) -> Loose,
    (Scissors, Scissors) -> Draw
  )

  private val myResponses: Map[(Shape, Outcome), Shape] = Map(
    (Rock, Draw) -> Rock,
    (Rock, Win) -> Paper,
    (Rock, Loose) -> Scissors,
    (Paper, Loose) -> Rock,
    (Paper, Draw) -> Paper,
    (Paper, Win) -> Scissors,
    (Scissors, Win) -> Rock,
    (Scissors, Loose) -> Paper,
    (Scissors, Draw) -> Scissors
  )

  private def pointsForStrategy1(abc: OpponentPlay, xyz: MyResponse): Int =
    outcomes(abc.shape, xyz.shape).points + xyz.shape.points

  private def pointsForStrategy2(abc: OpponentPlay, xyz: MyResponse): Int = xyz match
    case X => myResponses(abc.shape, Loose).points + Loose.points
    case Y => myResponses(abc.shape, Draw).points + Draw.points
    case Z => myResponses(abc.shape, Win).points + Win.points

  @main def solutionY2022Day02: Unit =
    val inputSeq = input.getLines().toSeq.filter(_.nonEmpty)

    val result1 = inputSeq
      .map(_.split(" "))
      .map(arr => (OpponentPlay.from(arr(0)), MyResponse.from(arr(1))))
      .map { case (abc, xyz) => pointsForStrategy1(abc, xyz) }
      .sum
    println(s"Total score for given first strategy: $result1")

    val result2 = inputSeq
      .map(_.split(" "))
      .map(arr => (OpponentPlay.from(arr(0)), MyResponse.from(arr(1))))
      .map { case (abc, xyz) => pointsForStrategy2(abc, xyz) }
      .sum
    println(s"Total score for given second strategy: $result2")

    input.close
