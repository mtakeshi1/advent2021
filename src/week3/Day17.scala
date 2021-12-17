package week3

object Day17 {
  def px(initialSpeed: Int)(t: Int): Double = {
    if (t == 0) 0
    else if (t > initialSpeed) px(initialSpeed)(initialSpeed)
    else {
      val fspeed = initialSpeed - (t - 1)
      (t / 2.0) * (initialSpeed + fspeed)
    }
  }

  def maxXDistance(initialSpeed: Int) = px(initialSpeed)(initialSpeed)

  def py(initialSpeed: Int)(t: Int): Double = {
    if (t == 0) 0
    else {
      val fspeed = initialSpeed - (t - 1)
      (t / 2.0) * (initialSpeed + fspeed)
    }
  }

  def minX(range: Range): Int = {
    1.until(range.max).find(x => maxXDistance(x) >= range.min).get
  }

  def isWithin(xRange: Range, yRange: Range)(x: Int, y: Int): Boolean = {
    val maxTX = solve2Equation(-0.5, (2 * x + 1) / 2, -xRange.min).filter(_ > 0).map(Math.ceil)
    val minTX = solve2Equation(-0.5, (2 * x + 1) / 2, -xRange.max).filter(_ > 0).map(Math.floor)

    val maxTY = solve2Equation(-0.5, (2 * y + 1) / 2, -yRange.min).filter(_ > 0).map(Math.ceil)
    val minTY = solve2Equation(-0.5, (2 * y + 1) / 2, -yRange.max).filter(_ > 0).map(Math.floor)
    val all: List[Double] = minTY ::: minTX ::: maxTX ::: maxTY
    if (all.isEmpty) false
    else {
      val minT: Int = all.min.toInt
      val maxT: Int = all.max.toInt
      val maybeInt = minT.to(maxT).find(t =>
        xRange.contains(px(x)(t)) && yRange.contains(py(y)(t)
        ))
      maybeInt.isDefined
    }
  }

  def highestYForSpeed(speed: Int): Int = {
    py(speed)(speed).asInstanceOf[Int]
  }

  def solve2Equation(a: Double, b: Double, c: Double): List[Double] = {
    val delta = b * b - 4 * a * c
    if (delta < 0) List()
    else {
      val s0 = (-b - Math.sqrt(delta)) / (2 * a);
      val s1 = (-b + Math.sqrt(delta)) / (2 * a);
      List(s0, s1).distinct
    }
  }

  def calcCandidatesY(range: Range): Range = {
    val maxY = Math.abs(range.min) - 1
    range.min.to(maxY)
  }

  def findRange(xRange: Range, yRange: Range) = {
    val cx = minX(xRange).to(xRange.max)
    val cy = calcCandidatesY(yRange)
    for {
      x <- cx
      y <- cy
      if isWithin(xRange, yRange)(x, y)
    } yield (x, y)
  }

  def main(args: Array[String]): Unit = {
    //    println(highestYForSpeed(149))
    val rangeX = 81.to(129)
    val rangeY = -150.to(-108)
    val r = findRange(rangeX, rangeY)
    println(highestYForSpeed(r.maxBy(_._2)._2))
//    r.foreach(println)
    println(r.size)
  }

}
