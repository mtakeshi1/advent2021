package week2

object Day9 {


  case class Input(matrix: Array[Array[Int]]) {

    def get(x: Int, y: Int): Option[Int] = {
      if (x < 0 || y < 0 || x >= matrix.length || y >= matrix(0).length) None
      else Some(matrix(x)(y))
    }

    def isDefined(x: Int, y: Int): Boolean = x >= 0 && y >= 0 && x < matrix.length && y < matrix(0).length

    def isDefined(t: (Int, Int)): Boolean = isDefined(t._1, t._2)

    def gather(x: Int, y: Int): List[Int] = List(get(x + 1, y), get(x - 1, y), get(x, y - 1), get(x, y + 1)).flatMap(_.toList)

    def neighboors(x: Int, y: Int): List[(Int, Int)] = List((x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1)).filter(isDefined)


    def isLowPoint(x: Int, y: Int) = gather(x, y).forall(c => c > matrix(x)(y))

    def findLowPoints() = for {
      x <- 0.until(matrix.length)
      y <- 0.until(matrix(0).length)
      if (isLowPoint(x, y))
    } yield matrix(x)(y)

    val allPositions = for {
      x <- 0.until(matrix.length)
      y <- 0.until(matrix(0).length)
    } yield (x, y)

    def findBasins(rem: List[(Int, Int)] = allPositions.toList): List[Set[(Int, Int)]] = {
      rem match {
        case Nil => List()
        case (x, y) :: rest if matrix(x)(y) != 9 => {
          val r = findBasinFrom(List((x, y)))
          val next = rest.filterNot(r.contains)
          r :: findBasins(next)
        }
        case _ :: rest => findBasins(rest)
      }
    }

    def findBasinFrom(fringe: List[(Int, Int)]): Set[(Int, Int)] = {
      var newFringe = fringe
      var visited: Set[(Int, Int)] = Set()
      while (!newFringe.isEmpty) {
        val next = newFringe.head
        if (!visited.contains(next)) {
          visited = visited + next
          newFringe = newFringe.tail ++ neighboors(next._1, next._2).filter(t => matrix(t._1)(t._2) != 9)
        } else {
          newFringe = newFringe.tail
        }
      }
      visited
    }

  }

  def read(file: String): Array[Array[Int]] = scala.io.Source.fromFile(file).getLines().map(_.trim).map(_.toCharArray.map(_ - '0')).toArray

  given Conversion[Array[Array[Int]], Input] = Input(_)


  def main(args: Array[String]): Unit = {
    val input = read("input9.txt")
    println(input.findBasins().map(_.size).sorted.reverse.take(3).product)
  }


}
