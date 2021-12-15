object Day5B {

  def parse(input: List[String]): List[((Int, Int), (Int, Int))] = {
    def firstTwo[A](arr: Array[A]): (A, A) = {
      if (arr.length == 1) sys.error("wtf?")
      (arr(0), arr(1))
    }

    def reorder(t: ((Int, Int), (Int, Int))) = {
      if (t._1._1 > t._2._1 || t._1._2 > t._2._2) (t._2, t._1)
      else t
    }

    input.map(_.split(" -> ")).map(firstTwo).map(t => (firstTwo(t._1.split(",").map(_.toInt)), firstTwo(t._2.split(",").map(_.toInt)))).map(reorder)
  }

  def inlined(t: (Int, Int), o: (Int, Int)) = t._1 == o._1 || t._2 == o._2

  def findOverlap(input: List[((Int, Int), (Int, Int))]) = {
    val map = scala.collection.mutable.Map[(Int, Int), Int]()
    input.foreach { tuple =>
      val incX = if (tuple._1._1 == tuple._2._1) 0 else if (tuple._1._1 < tuple._2._1) 1 else -1
      val incY = if (tuple._1._2 == tuple._2._2) 0 else if (tuple._1._2 < tuple._2._2) 1 else -1

      def rec(src: (Int, Int), target: (Int, Int)): Unit = {
        val x = src._1
        val y = src._2
        map.update((x, y), 1 + map.getOrElse((x, y), 0))
        if (src != target) {
          rec((x + incX, y + incY), target)
        }
      }

      rec(tuple._1, tuple._2)

    }
    //        map.update((x, y), 1+ map.getOrElse((x, y),0))
    map
  }

  def parseFile(file: String) = parse(scala.io.Source.fromFile(file).getLines().map(_.trim).toList)

  def main(args: Array[String]): Unit = {
    val map = findOverlap(parseFile("input5.txt"))

    println(map.values.count(_ >= 2))
  }


}
