package io.c6.aoc.y2022

import io.c6.aoc.util.BaseSolution
import io.c6.aoc.util.Day.*
import io.c6.aoc.util.InputType.*
import io.c6.aoc.util.Year.*

import scala.language.implicitConversions

@main def _2022_02: Unit = Day02()

object Day02 extends BaseSolution(_2022, _02):
  import StringConversions.given

  private sealed trait Outcome(val points: Int)

  private object Win extends Outcome(6)

  private object Draw extends Outcome(3)

  private object Loose extends Outcome(0)

  private sealed trait Shape(val points: Int)

  private object Rock extends Shape(1)

  private object Paper extends Shape(2)

  private object Scissors extends Shape(3)

  private sealed trait OpponentPlay(val shape: Shape)

  private object A extends OpponentPlay(Rock)

  private object B extends OpponentPlay(Paper)

  private object C extends OpponentPlay(Scissors)

  private sealed trait MyResponse(val shape: Shape)

  private object X extends MyResponse(Rock)

  private object Y extends MyResponse(Paper)

  private object Z extends MyResponse(Scissors)

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

  private type PlayPair = (OpponentPlay, MyResponse)

  private object StringConversions:
    private given toOpponentPlay: Conversion[String, OpponentPlay] = str => str match
      case "A" => A
      case "B" => B
      case _ => C

    private given toMyResponse: Conversion[String, MyResponse] = str => str match
      case "X" => X
      case "Y" => Y
      case _ => Z

    given Conversion[String, PlayPair] = str => str.split(" ") match
      case Array(abc, xyz) => toOpponentPlay(abc) -> toMyResponse(xyz)

  private def pointsForStrategy1(playPair: PlayPair): Int = playPair match
    case abc -> xyz =>
      outcomes(abc.shape, xyz.shape).points + xyz.shape.points

  private def pointsForStrategy2(playPair: PlayPair): Int = playPair match
    case abc -> xyz => xyz match
      case X => myResponses(abc.shape, Loose).points + Loose.points
      case Y => myResponses(abc.shape, Draw).points + Draw.points
      case Z => myResponses(abc.shape, Win).points + Win.points

  protected override def part1Solution: Seq[String] => Unit = input =>
    val result = input.map(pointsForStrategy1(_)).sum
    println(s"Result for part 1: $result")

  protected override def part2Solution: Seq[String] => Unit = input =>
    val result = input.map(pointsForStrategy2(_)).sum
    println(s"Result for part 2: $result")
