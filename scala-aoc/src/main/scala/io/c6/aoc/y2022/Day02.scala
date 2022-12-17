package io.c6.aoc.y2022

import io.c6.aoc.BaseSolution
import io.c6.aoc.BaseSolution.*
import io.c6.aoc.Day.*
import io.c6.aoc.Year.*
import io.c6.aoc.InputType.*

object Day02 extends BaseSolution:

  protected override val part1InputFileName: String = getInputFileName(_2022, _02, A1)

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

  import MyResponse.*
  import OpponentPlay.*
  import Outcome.*
  import Shape.*

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

  private def parse(line: String): (OpponentPlay, MyResponse) = line.split(" ") match
    case Array(abc, xyz) => OpponentPlay.from(abc) -> MyResponse.from(xyz)

  protected override def part1Solution: Seq[String] => Unit = { input =>
    val result = input.map(parse).map(pointsForStrategy1).sum
    println(s"Result for part 1: $result")
  }

  protected override def part2Solution: Seq[String] => Unit = { input =>
    val result = input.map(parse).map(pointsForStrategy2).sum
    println(s"Result for part 2: $result")
  }

@main def runDay02: Unit = Day02.run
