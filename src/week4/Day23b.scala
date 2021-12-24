package week4

import java.util.Date
import scala.collection.immutable.TreeMap

object Day23b {

  /*
   #...........#
   ###A#C#C#D###
     #B#D#A#B#
     #########

   #123456789AB#
   ###C#G#K#O###
     #D#H#L#P#
     #E#I#M#Q#
     #F#J#N#R#
     #########
   */

  val defaultCells = Seq(
    Cell(1, Seq(2), canStop = true),
    Cell(2, Seq(1, 3), canStop = true),
    Cell(3, Seq(2, 4, 'C'), canStop = false),
    Cell(4, Seq(3, 5), canStop = true),
    Cell(5, Seq(4, 6, 'G'), canStop = false),
    Cell(6, Seq(5, 7), canStop = true),
    Cell(7, Seq(6, 8, 'K'), canStop = false),
    Cell(8, Seq(7, 9), canStop = true),
    Cell(9, Seq(8, 'A', 'O'), canStop = false),
    Cell('A', Seq(9, 'B'), canStop = true),
    Cell('B', Seq('A'), canStop = true),

    Cell('C', Seq(3, 'D'), roomName = Some("A"), canStop = true),
    Cell('D', Seq('C', 'E'), roomName = Some("A"), canStop = true),
    Cell('E', Seq('D', 'F'), roomName = Some("A"), canStop = true),
    Cell('F', Seq('E'), roomName = Some("A"), canStop = true),

    Cell('G', Seq(5, 'H'), roomName = Some("B"), canStop = true),
    Cell('H', Seq('G', 'I'), roomName = Some("B"), canStop = true),
    Cell('I', Seq('H', 'J'), roomName = Some("B"), canStop = true),
    Cell('J', Seq('I'), roomName = Some("B"), canStop = true),

    Cell('K', Seq(7, 'L'), roomName = Some("C"), canStop = true),
    Cell('L', Seq('K', 'M'), roomName = Some("C"), canStop = true),
    Cell('M', Seq('L', 'N'), roomName = Some("C"), canStop = true),
    Cell('N', Seq('M'), roomName = Some("C"), canStop = true),

    Cell('O', Seq(9, 'P'), roomName = Some("D"), canStop = true),
    Cell('P', Seq('O', 'Q'), roomName = Some("D"), canStop = true),
    Cell('Q', Seq('P', 'R'), roomName = Some("D"), canStop = true),
    Cell('R', Seq('Q'), roomName = Some("D"), canStop = true)
  ).map(cell => cell.id -> cell).toMap

  /*
   #123456789AB#
   ###C#G#K#O###
     #D#H#L#P#
     #E#I#M#Q#
     #F#J#N#R#
     #########

#############
#...........#
###A#C#C#D###
  #D#C#B#A#
  #D#B#A#C#
  #B#D#A#B#
  #########
  */
  val startingPositions: Map[CellId, Amp] = Map[CellId, Amp](
    'C'.toInt -> Amp("A", 1),
    'M'.toInt -> Amp("A", 1),
    'N'.toInt -> Amp("A", 1),
    'P'.toInt -> Amp("A", 1),

    'F'.toInt -> Amp("B", 10),
    'I'.toInt -> Amp("B", 10),
    'L'.toInt -> Amp("B", 10),
    'R'.toInt -> Amp("B", 10),

    'G'.toInt -> Amp("C", 100),
    'H'.toInt -> Amp("C", 100),
    'K'.toInt -> Amp("C", 100),
    'Q'.toInt -> Amp("C", 100),

    'D'.toInt -> Amp("D", 1000),
    'E'.toInt -> Amp("D", 1000),
    'J'.toInt -> Amp("D", 1000),
    'O'.toInt -> Amp("D", 1000)
  );

  def colForAmp(s: String): Seq[Cell] = {
    defaultCells.values.filter(_.roomName == Some(s)).toSeq.sortBy(_.id)
  }

  def sanityTest = {
    defaultCells.values.foreach(c => {
      val neigh = c.neighboors(defaultCells)
      assert(neigh.forall(n => n.neighboors(defaultCells).contains(c)), s"neighboors of $c : ${neigh.filterNot(_.neighboors(defaultCells).contains(c))}")
    })
    assert(colForAmp("A").map(_.id).sorted == Seq[Int]('C', 'D', 'E', 'F'))
    assert(colForAmp("B").map(_.id).sorted == Seq[Int]('G', 'H', 'I', 'J'))
    assert(colForAmp("C").map(_.id).sorted == Seq[Int]('K', 'L', 'M', 'N'))
    assert(colForAmp("D").map(_.id).sorted == Seq[Int]('O', 'P', 'Q', 'R'))
    println("sanity check ok")
  }

  opaque type CellId = Int

  case class Amp(name: String, baseCost: Int) {
    def isOnHallway(amps: Map[CellId, Amp]) = {
      val id = amps.find(t => t._2 eq this).map(_._1).get
      defaultCells(id).isHallway
    }
  }

  case class Cell(id: CellId, neighoorsIds: Seq[CellId], roomName: Option[String] = None, canStop: Boolean) {
    def neighboors(allPositions: Map[CellId, Cell]): Seq[Cell] = neighoorsIds.map(id => allPositions(id))

    def room(name: String): Boolean = roomName.filter(_ == name).isDefined

    def formatId: Char = (if (id >= 'A' && id <= 'Z') 'A' + (id - 'A') else '0' + (id - '0')).asInstanceOf[Char]

    override def toString: String = s"Cell( $formatId ${neighoorsIds.mkString("[", ",", "]")} ${roomName.getOrElse("")} )"

    def isEmpty(amps: Map[CellId, Amp]) = !amps.isDefinedAt(this.id)

    def hasCorrectAmp(amps: Map[CellId, Amp]) = {
      assert(roomName.isDefined)
      amps.get(this.id).filter(_.name == this.roomName.get).isDefined
    }

    def isRoom = roomName.isDefined
    def isHallway = !isRoom

    def canStop(amp: Amp, amps: Map[CellId, Amp]): Boolean = {
      if (!canStop) false
      else if(this.isHallway) {
        !amp.isOnHallway(amps)
      } else if(this.isRoom){
        this.roomName.get == amp.name && colForAmp(amp.name).forall(cell => cell.isEmpty(amps) || cell.hasCorrectAmp(amps))
      } else {
        sys.error("should never end up here =(")
        true
      }
    }


    val cachedHash: Int = id << 16 + neighoorsIds.hashCode() ^ roomName.hashCode();

    override def hashCode(): CellId = cachedHash
  }

  case class State(cells: Map[CellId, Cell], amps: Map[CellId, Amp]) {

    assert(amps.size == 16)
    assert(amps.keys.forall(cells.isDefinedAt))
    val cachedHash: Int = cells.hashCode() + amps.hashCode()

    override def hashCode(): CellId = cachedHash

    def correctAmpsNamed(name: String) = {
      colForAmp(name).sortBy(-_.id).takeWhile(cell => amps.get(cell.id).map(_.name) == Some(name)).size
    }

    val correctAmps = correctAmpsNamed("A") + correctAmpsNamed("B") + correctAmpsNamed("C") + correctAmpsNamed("D")


    def isSolved = amps.forall((cellId, amp) => cells(cellId).room(amp.name))

    def findMoves(amp: Amp, cell: Cell, visited: Set[Cell], baseCost: Int = 1): Seq[(Cell, Int)] = {
      val possible = cell.neighboors(cells).filterNot(visited.contains).filter(maybe => {
        amps.get(maybe.id).isEmpty
      })
      val stopsOnly = possible.filter(_.canStop(amp, amps))
      val newVisited = visited ++ possible
      val rec: Seq[(Cell, CellId)] = possible.flatMap(another => {
        findMoves(amp, another, newVisited, baseCost + 1)
      })
      stopsOnly.map(_ -> baseCost) ++ rec
    }

    def nextStates(currentCost: Int = 0, visisted: scala.collection.Map[State, Int] = Map()): Map[State, Int] = {
      amps.flatMap(t => {
        val cell = cells(t._1)
        val nextMoves: Seq[(Cell, CellId)] = findMoves(t._2, cell, Set(cell)).map(move => (move._1, move._2 * t._2.baseCost))
        val value: Seq[(State, Int)] = nextMoves.map(move => {
          val newPositions: Map[CellId, Amp] = amps.removed(t._1).updated(move._1.id, t._2)
          (State(cells, newPositions), currentCost + move._2)
        })
        value.filter(t => !visisted.contains(t._1) || visisted(t._1) > t._2)
      })
    }

    def cellToString(i: CellId) = {
      amps.get(i).map(_.name).getOrElse(".")
    }

    override def toString: String = {
      "\n#############\n" +
        1.to(9).map(cellToString).mkString("#", "", "") + 'A'.to('B').map(c => cellToString(c.toInt)).mkString("", "", "#") +
        (s"""
            |###${cellToString('C')}#${cellToString('G')}#${cellToString('K')}#${cellToString('O')}###
            |  #${cellToString('D')}#${cellToString('H')}#${cellToString('L')}#${cellToString('P')}#
            |  #${cellToString('E')}#${cellToString('I')}#${cellToString('M')}#${cellToString('Q')}#
            |  #${cellToString('F')}#${cellToString('J')}#${cellToString('N')}#${cellToString('R')}#
            |  #########
            |""").stripMargin
    }

  }

  def main(args: Array[String]): Unit = {
    sanityTest

    val state = State(defaultCells, startingPositions)
    val visitedStates = scala.collection.mutable.Map[State, Int]()
    val order = Ordering.by[List[((Int, State))], Long](t => -(t.head._1.toLong << 4 )) //+ t.head._2.correctAmps
    val fringe = scala.collection.mutable.PriorityQueue[List[(Int, State)]](List(0 -> state))(order)
    var time = System.currentTimeMillis() + 30000;
    while (!fringe.isEmpty) {
      val first = fringe.dequeue()
      if (first.head._2.isSolved) {
        first.reverse.foreach(println)
        println(first.head._1)
        return;
      } else if (!visitedStates.contains(first.head._2)) {
        visitedStates.put(first.head._2, first.head._1)
        val nextStates: Map[State, Int] = first.head._2.nextStates(first.head._1, visitedStates)

        fringe.addAll(nextStates.toSeq.filter(_._2 < 54277).map(t => (t._2, t._1) :: first))
      }
      if (System.currentTimeMillis() > time) {
        println(s"${new Date()} fringe size: ${fringe.size} allStatesSize: ${visitedStates.size} current best score: ${first.head._1}")
        time = System.currentTimeMillis() + 10000;
      }

    }
  }

}
