package week3

import scala.annotation.tailrec

object Day21 {

  case class Die(n: Int = 0) {

    def norm(i: Int) = 1 + i % 100

    def roll3(): (Int, Die) = (norm(n) + norm(n + 1) + norm(n + 2), Die(n + 3))


  }

  case class Player(position: Int, points: Int = 0) {


    def norm(i: Int) = 1 + i % 10

    def next(die: Die) = {
      val (steps, newDie) = die.roll3()
      val newPos = norm(this.position - 1 + steps)
      (newDie, Player(newPos, points + newPos))
    }

    def won = points >= 1000

  }


  def run(p0: Player, p1: Player, die: Die, turns: Int = 0): Int = {
    if (p0.won) {
      p1.points * turns
    } else if (p1.won) {
      p0.points * turns
    } else {
      val (nd, np) = p0.next(die)
      run(p1, np, nd, turns + 3) + 0
    }
  }

  def sanityCheck() = {

    var player2 = Player(8)
    var die = Die()

    assert(die.roll3()._1 == 6)
    assert(die.roll3()._2.n == 3)

    val d2 = die.roll3()._2
    assert(d2.roll3()._1 == 15)


    val d3 = d2.roll3()._2
    assert(d3.roll3()._1 == 24)

    var player1 = Player(4)
    val p1_1 = player1.next(die)
    assert(p1_1._1 == d2)
    assert(p1_1._2 == Player(10, 10))
    val p2_1 = player2.next(d2)
    assert(p2_1._1 == d3)
    assert(p2_1._2 == Player(3, 3))

  }

  def norm(i: Int) = 1 + ((i - 1) % 10)

  def findPeriod(initial: Int, deltaScore: Int): List[Int] = {


    val sample = 0.until(12).map(initial + deltaScore * _).map(norm).toList
    sample.tail.takeWhile(e => e != sample.head) ::: sample.head :: List()
  }

  def scoreFor(initial: Int, deltaScore: Int, turns: Int) = {
    val period = findPeriod(initial, deltaScore)
    val complete = period.sum * (turns / period.length)
    val rem = period.take(turns % period.length).sum
    complete + rem
  }

  case class QuantumPlayerState(count: BigInt = 1, position: Int, score: Int = 0) {
    def next(possibleDelta: PossibleDelta) = {
      val nextPositon = norm(this.position + possibleDelta.delta)
      QuantumPlayerState(this.count * possibleDelta.n, nextPositon, score + nextPositon)
    }

    def won = score >= 21
  }

  case class PossibleDelta(n: Int, delta: Int)

  val possibilities: Seq[PossibleDelta] = {
    val tmp = for {x <- 1.to(3); y <- 1.to(3); z <- 1.to(3)} yield (x + y + z)
    tmp.min.to(tmp.max).map(t => PossibleDelta(tmp.count(_ == t), t))
  }


  case class QuantumGame(p0: Seq[QuantumPlayerState], p1: Seq[QuantumPlayerState], p0wins: BigInt = 0, p1wins: BigInt = 0) {

    def turn(): QuantumGame = {
      val newP0 = for {
        state <- p0
        delta <- possibilities
      } yield state.next(delta)

      val results = newP0.groupBy(_.won)
      val newWins = results.get(true).getOrElse(List()).map(_.count).sum * p1.map(_.count).sum
      val qg = QuantumGame(p1, results.get(false).getOrElse(List()), p1wins, p0wins + newWins)

      val before = p0.map(_.count).sum * 27
      val after = newP0.map(_.count).sum
      assert(before == after)

      qg
    }

    def totalGames = p0.map(_.count).sum + p1.map(_.count).sum + p0wins + p1wins

    override def toString: String = s"total: $totalGames, p0: $p0wins p1: $p1wins"

    def allFinished = p0.isEmpty || p1.isEmpty

  }


  def turnsUntilWin(initial: Int, delta: Int) = 1.until(1000).find(t => scoreFor(initial, delta, t) >= 21).get


  def main(args: Array[String]): Unit = {
    sanityCheck()

    var player1 = Player(10)
    var player2 = Player(6)
    var die = Die()

    println(run(player1, player2, die))

    println(findPeriod(10, 3))

    assert(scoreFor(10, 3, 1) == 3)

    assert(scoreFor(10, 3, 2) == 9)
    assert(scoreFor(10, 3, 12) == 64)
    assert(scoreFor(3, 6, 55) == 275)

    var game = QuantumGame(List(QuantumPlayerState(count = 1, position = 10)), List(QuantumPlayerState(count = 1, position = 6)))

    while (!game.allFinished) {
      game = game.turn();
      println(game)
    }

    println(if(game.p0wins > game.p1wins) game.p0wins else game.p1wins)

    //145727874122839
    //306719685234774

  }

}
