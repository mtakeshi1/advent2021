object Day4 {

  case class Board(elements: List[List[Int]]) {
    def select(n: Int) = Board(elements.map(l => l.map(d => if (d == n) -1 else d)))

    def col(c: Int) = elements.map(_ (c))

    def won = elements.exists(l => l.forall(n => n == -1)) || 0.until(elements.size).map(col).exists(_.forall(_ == -1))

    override def toString = elements.map(_.mkString("", " ", "")).mkString("", "\n", "")

    def score(lastNumber: Int) = {
      val sum = elements.flatMap(l => l).filter(_ != -1).sum
      sum * lastNumber
    }

  }

  case class Input(raffle: List[Int], boards: List[Board]) {
    def solve: Int = {
      val n = raffle.head
      val newBoards = boards.map(_.select(n))
      val finished: Option[Board] = newBoards.find(_.won)
      finished match {
        case None => Input(raffle.tail, newBoards).solve
        case Some(winning) => winning.score(n)
      }
    }

    def lastToWin: Int = {
      val n = raffle.head
      val newBoards = boards.map(_.select(n)).filter(!_.won)
      if (newBoards.isEmpty) {
        boards.head.select(n).score(n)
      } else {
        Input(raffle.tail, newBoards).lastToWin
      }
    }

    def next: Input = {
      if (boards.find(_.won).isDefined) sys.error("already won")
      val n = raffle.head
      val newBoards = boards.map(_.select(n))
      val finished: Option[Board] = boards.find(_.won)
      if (finished.isDefined) println("won");
      Input(raffle.tail, newBoards)
    }

  }


  def parseRaffle(l: List[String]): List[Int] = l.head.split(",").map(_.trim).filter(_.size > 0).map(_.toInt).toList


  def parseBoards(input: List[String]): List[Board] =
    if (input.size < 5) List()
    else {
      Board(input.take(5).map(ll => ll.split(" ").map(_.trim).filter(_.size > 0).map(_.toInt).toList)) :: parseBoards(input.drop(6))
    }

  def parse(l: List[String]) = Input(parseRaffle(l), parseBoards(l.drop(2)))

  def parseFile(s: String) = parse(scala.io.Source.fromFile(s).getLines.map(_.trim).toList)

  def main(args: Array[String]): Unit = {
    println(parseFile("input.txt").solve)
  }
}
