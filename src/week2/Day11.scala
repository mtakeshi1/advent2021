package week2

object Day11 {


  def parse(file: String): List[List[Int]] = scala.io.Source.fromFile(file).getLines().map(_.trim).map(_.toList.map(_ - '0')).toList

  def applyFlash(x: Int, y: Int, input: List[List[Int]]): List[List[Int]] = {
    var r = input
    val toUpdate: Iterable[(Int, Int)] = for {
      nx <- -1.to(1).map(_ + x)
      ny <- -1.to(1).map(_ + y)
      if nx >= 0 && nx < 10 && ny >= 0 && ny < 10
    } yield (nx, ny)
    toUpdate.foldLeft(input)((in, t) => in.updated(t._1, in(t._1).updated(t._2, in(t._1)(t._2) + 1)))
  }

  val allIndexes: Seq[(Int, Int)] = 0.until(10).flatMap(x => 0.until(10).map(y => (x, y)))

  def applyFlashes(input: List[List[Int]]): List[List[Int]] = {
    var copy = input.map(_.map(_ + 1))
    var justShined = allIndexes.find(t => copy(t._1)(t._2) > 9 && input(t._1)(t._2) < 10)
    while (justShined.isDefined) {
      val (x, y) = justShined.get
      copy = applyFlash(x, y, copy)
      justShined = allIndexes.find(t => copy(t._1)(t._2) > 9 && input(t._1)(t._2) < 10)
    }
    copy
  }

  def applyFlashR(acc: List[List[Int]], shined: Set[(Int, Int)] = Set()): List[List[Int]] = {
    allIndexes.find(t => acc(t._1)(t._2) > 9 && !shined.contains(t)) match {
      case Some((x, y)) => applyFlashR(applyFlash(x, y, acc), shined + ((x, y)))
      case None => acc
    }
  }


  def countFlashes(input: List[List[Int]]): (Int, List[List[Int]]) = {
    val counts = input.map(_.count(_ > 9)).sum
    (counts, input.map(_.map(el => if (el > 9) 0 else el)))
  }

  def step(input: List[List[Int]]) = {
    val copy = applyFlashR(input.map(_.map(_ + 1)))
    countFlashes(copy)
  }

  def steps(n: Int, input: List[List[Int]]): Int = {
    if (n == 0) 0
    else {
      val (c, i) = step(input)
      c + steps(n - 1, i)
    }
  }

  def findSync(input: List[List[Int]], n: Int = 0): Int = {
    if (n > 1000) sys.error("err")
    if (input.forall(_.forall(_ == 0))) 0
    else 1 + findSync(step(input)._2)
  }

  def main(args: Array[String]): Unit = {
    val input = parse("input11.txt")
    println(steps(100, input))
    println(findSync(input))
  }

}
