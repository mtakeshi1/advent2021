package week3

object Day15 {

  def parse(file: String) = {
    Input(scala.io.Source.fromFile(file).getLines().map(_.trim).map(_.map(_ - '0').toArray).toArray)
  }

  case class Input(dangerLevels: Array[Array[Int]]) {
    val destination = (dangerLevels.size - 1, dangerLevels(0).size - 1)

    trait Fringe {
      def hasVisited(t: (Int, Int)): Boolean

      def updateCost(t: (Int, Int), nc: Int): Fringe

      def next(): ((Int, Int, Int), Fringe)

      def isEmpty: Boolean
    }

    case class FringeImpl(knownCosts: Map[(Int, Int), Int] = Map(), queue: List[(Int, Int, Int)] = List()) extends Fringe {
      override def hasVisited(t: (Int, Int)): Boolean = knownCosts.contains(t)

      override def isEmpty: Boolean = queue.isEmpty

      override def updateCost(t: (Int, Int), nc: Int): Fringe = {
        knownCosts.get(t) match {
          case Some(value) if value > nc => FringeImpl(knownCosts.updated(t, nc), ((t._1, t._2, nc) :: queue).sortBy(_._3))
          case None => FringeImpl(knownCosts.updated(t, nc), ((t._1, t._2, nc) :: queue).sortBy(_._3))
          case _ => this
        }
      }

      override def next(): ((Int, Int, Int), Fringe) = {
        val first = queue.head
        (first, FringeImpl(knownCosts, queue.tail))
      }

    }

    case class PartialPath(pathSoFar: List[(Int, Int)] = List(), costFoFar: Int = 0) {
      def prepend(p: (Int, Int)) = PartialPath(p :: pathSoFar, costFoFar + dangerLevels(p._1)(p._2))
    }

    def neighboorsOf(p: (Int, Int)): Iterable[(Int, Int)] = {
      List((p._1 - 1, p._2), (p._1 + 1, p._2), (p._1, p._2 - 1), (p._1, p._2 + 1)).filter(_._1 >= 0).filter(_._2 >= 0).filter(_._1 <= destination._1).filter(_._2 <= destination._2)
    }

    def cost(p: (Int, Int)) = dangerLevels(p._1)(p._2)

    def pickBest(a: Option[PartialPath], b: Option[PartialPath]): Option[PartialPath] = {
      (a, b) match {
        case (None, None) => None
        case (None, r) => r
        case (r, None) => r
        case (Some(l), Some(r)) => if (l.costFoFar < r.costFoFar) Some(l) else Some(r)
      }
    }

    def findPath(): Int = {
      var visiting = (0, 0)
      var costSoFar = 0
      var fringe: Fringe = FringeImpl(knownCosts = Map(visiting -> 0))
      while (visiting != destination) {
        fringe = neighboorsOf(visiting).filterNot(fringe.hasVisited).foldRight(fringe)((p, nf) => nf.updateCost(p, costSoFar + cost(p)))
        val next = fringe.next()

        visiting = (next._1._1, next._1._2)
        costSoFar = next._1._3
        fringe = next._2
      }
      costSoFar
    }

    def largeInput(): Input = {
      def clean(i: Int) = if (i > 9) i % 9 else i

      val newInput: Array[Array[Int]] = Array.ofDim(this.dangerLevels.length * 5, this.dangerLevels(0).length * 5)
      0.until(5).foreach(mx => 0.until(5).foreach(my => {
        0.until(dangerLevels.length).foreach(x => 0.until(dangerLevels(0).length).foreach(y => {
          newInput(mx * dangerLevels.length + x)(my * dangerLevels(0).length + y) = clean(dangerLevels(x)(y) + mx + my)
        }))
      }))
      Input(newInput)
    }

    def formatDangerLevels() = {
      dangerLevels.map(_.mkString("")).mkString("\n")
    }

  }

  def main(args: Array[String]): Unit = {
    val input = parse("input15.txt")
    //    println(input.largeInput().formatDangerLevels())
    println(input.largeInput().findPath())
  }


}
