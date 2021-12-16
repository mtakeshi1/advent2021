package week2

object Day13 {

  object Paper {
    def apply(dots: Set[(Int, Int)]) = {
      val maxX = dots.map(_._1).max
      val maxY = dots.map(_._2).max
      new Paper(dots, maxX, maxY)
    }
  }

  case class Paper(dots: Set[(Int, Int)], maxX: Int, maxY: Int) {

    def foldY(y: Int) = {
      val intact = dots.filter(_._2 < y)
      val toInvert = dots.filter(_._2 > y)
      val foldSize = maxY - y

      val inverted = toInvert.map(t => (t._1, y - (t._2 - y)))
      assert(inverted.forall(_._2 <= y))
      Paper(intact ++ inverted, maxX, y - 1)
    }

    def foldX(x: Int) = {
      val intact = dots.filter(_._1 < x)
      val toInvert = dots.filter(_._1 > x)
      val foldSize = maxX - x

      val inverted = toInvert.map(t => (x - (t._1 - x), t._2))
      assert(inverted.forall(_._1 <= x))
      assert(inverted.forall(_._1 >= 0))
      Paper(intact ++ inverted, x - 1, maxY)
    }

    def fold(t: (Int, Int)) = {
      t match {
        case (0, y) => foldY(y)
        case (x, 0) => foldX(x)
        case _ => sys.error(s"fold error: $t")
      }
    }

    def print(): String = {
      println(s"dots: ${dots.size} maxX: $maxX, maxY: $maxY")
      0.to(maxY).map(y => {
        0.to(maxX).map(x => {
          if (dots.contains((x, y))) "#" else "."
        }).mkString("")
      }).mkString("\n")
    }

  }


  def parseInstruction(line: String): (Int, Int) = {
    val i = line.split("=")(1).toInt
    if (line.contains("y=")) (0, i) else (i, 0)
  }


  def parse(file: String): (Paper, List[(Int, Int)]) = {
    val lines = scala.io.Source.fromFile(file).getLines().map(_.trim)
    val dots = Paper(lines.takeWhile(_.size > 0).map(_.split(",")).map(arr => (arr(0).toInt, arr(1).toInt)).toSet)
    val instructions = lines.map(parseInstruction).toList
    (dots, instructions)
  }

  def main(args: Array[String]): Unit = {
    val (paper, instructions) = parse("input13.txt")

    def applyFolds(p: Paper, ins: List[(Int, Int)]): Paper = {
      println(p.print())
      println(p.dots.size)

      ins match {
        case Nil => p
        case h :: t => applyFolds(p.fold(h), t)
      }
    }

    val r = applyFolds(paper, instructions)
    println(r.print())
    //    println(r.dots.size)
    // 1 - shoult not apply all instructions
    // 2 - folds are not necessarily in half
    //    val paper = Paper(Set((0, 4), (0, 5)), 0, 5)
    //    println(paper.print())
    //    println("------------------------------------------------------------")
    //    println(paper.foldY(7).print())
    //    println(paper.fold((0, 3)).print())
    //    println(paper.fold((0, 3)).dots.size)
    //    println("------------------------------------------------------------")
    //    val afterOne = paper.fold(instructions.head)
    //    println(afterOne.dots.size)
  }


}
