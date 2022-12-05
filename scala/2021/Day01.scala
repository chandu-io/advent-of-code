val input = io.Source.fromFile("input/day-01-input.txt")

def countDepthIncrements(depths: Seq[Int]): Int =
  depths.sliding(2).count {
    case a :: b :: Nil => b > a
    case _ => false
  }

@main def solution: Unit =
  val depths = input.getLines().map(_.toInt).toSeq
  val result1 = countDepthIncrements(depths)
  println(result1)

  val depthSums = depths.sliding(3).map(_.sum).toSeq
  val result2 = countDepthIncrements(depthSums)
  println(result2)

  input.close
