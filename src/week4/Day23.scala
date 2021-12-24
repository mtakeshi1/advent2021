package week4

import java.util.Date
import scala.collection.immutable.TreeMap

object Day23 {

  /*
   #...........#
   ###A#C#C#D###
     #B#D#A#B#
     #########

   #123456789AB#
   ###C#E#G#I###
     #D#F#H#J#
     #########
   */

  val defaultCells = Seq(
    Cell(1, Seq(2), canStop = true),
    Cell(2, Seq(1, 3), canStop = true),
    Cell(3, Seq(2, 4, 'C'), canStop = false),
    Cell(4, Seq(3, 5), canStop = true),
    Cell(5, Seq(4, 6, 'E'), canStop = false),
    Cell(6, Seq(5, 7), canStop = true),
    Cell(7, Seq(6, 8, 'G'), canStop = false),
    Cell(8, Seq(7, 9), canStop = true),
    Cell(9, Seq(8, 'A', 'I'), canStop = false),
    Cell('A', Seq(9, 'B'), canStop = true),
    Cell('B', Seq('A'), canStop = true),

    Cell('C', Seq(3, 'D'), roomName = Some("A"), canStop = true),
    Cell('D', Seq('C'), roomName = Some("A"), canStop = true),

    Cell('E', Seq(5, 'F'), roomName = Some("B"), canStop = true),
    Cell('F', Seq('E'), roomName = Some("B"), canStop = true),

    Cell('G', Seq(7, 'H'), roomName = Some("C"), canStop = true),
    Cell('H', Seq('G'), roomName = Some("C"), canStop = true),

    Cell('I', Seq(9, 'J'), roomName = Some("D"), canStop = true),
    Cell('J', Seq('I'), roomName = Some("D"), canStop = true)
  ).map(cell => cell.id -> cell).toMap

  val startingPositions = Map[CellId, Amp](
    'C'.toInt -> Amp("A", 1),
    'H'.toInt -> Amp("A", 1),

    'D'.toInt -> Amp("B", 10),
    'J'.toInt -> Amp("B", 10),

    'E'.toInt -> Amp("C", 100),
    'G'.toInt -> Amp("C", 100),

    'F'.toInt -> Amp("D", 1000),
    'I'.toInt -> Amp("D", 1000)
  );

  def sanityTest = {
    defaultCells.values.foreach(c => {
      val neigh = c.neighboors(defaultCells)
      assert(neigh.forall(n => n.neighboors(defaultCells).contains(c)), s"neighboors of $c : ${neigh.filterNot(_.neighboors(defaultCells).contains(c))}")
    })
  }

  opaque type CellId = Int

  case class Amp(name: String, baseCost: Int)
  def colForAmp(s: String): Seq[Cell] = {
    defaultCells.values.filter(_.roomName == Some(s)).toSeq.sortBy(_.id)
  }


  case class Cell(id: CellId, neighoorsIds: Seq[CellId], roomName: Option[String] = None, canStop: Boolean) {
    def neighboors(allPositions: Map[CellId, Cell]): Seq[Cell] = neighoorsIds.map(id => allPositions(id))

    def room(name: String): Boolean = roomName.filter(_ == name).isDefined

    def canStop(amp: Amp, amps: Map[CellId, Amp]): Boolean = {
      if(!canStop) false
      else if (roomName.isEmpty) true
      else if(roomName.get == amp.name) {
        val columns = colForAmp(amp.name).filter(_.id > this.id)
        if(columns.exists(cell => !amps.isDefinedAt(cell.id) || amps(cell.id).name != roomName.get)) {
          false
        } else {
//          println(s"woot? $columns $roomName")
          true
        }
        //        !columns.exists(cell => !amps.isDefinedAt(cell.id) || )
        //        columns.forall(cell => amps(cell.id).name == roomName.get)
      } else false
    }


    val cachedHash: Int = id << 16 + neighoorsIds.hashCode() ^ roomName.hashCode();

    override def hashCode(): CellId = cachedHash
  }

  case class State(cells: Map[CellId, Cell], amps: Map[CellId, Amp]) {

    val cachedHash: Int = cells.hashCode() + amps.hashCode()

    override def hashCode(): CellId = cachedHash

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

    def nextStates(currentCost: Int = 0, visisted: Map[State, Int] = Map()): Map[State, Int] = {
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
            |###${cellToString('C')}#${cellToString('E')}#${cellToString('G')}#${cellToString('I')}###
            |  #${cellToString('D')}#${cellToString('F')}#${cellToString('H')}#${cellToString('J')}#
            |  #########
            |""").stripMargin
    }

  }

  def main(args: Array[String]): Unit = {
    sanityTest

    val state = State(defaultCells, startingPositions)
    var visitedStates = Map[State, Int]()
    val order = Ordering.by[(Int, State), Int](t => -t._1)
    val fringe = scala.collection.mutable.PriorityQueue[(Int, State)](0 -> state)(order)
    var time = System.currentTimeMillis() + 30000;
    while (!fringe.isEmpty) {
      val first = fringe.dequeue()
      if (first._2.isSolved) {
        println(first._2)
        println(first._1)
        return;
      } else if (!visitedStates.contains(first._2)) {
        visitedStates = visitedStates.updated(first._2, first._1)
        val nextStates: Map[State, Int] = first._2.nextStates(first._1, visitedStates)

        fringe.addAll(nextStates.toSeq.map(t => (t._2, t._1)).filter(_._1 < 15548))
//        fringe = fringe.filter(_._1 < 15548).filter(t => t._1 <= visitedStates(t._2))
      }
      //      val nextFringe = sorted.tail
      //      fringe = (nextFringe ++ nextStates.toSeq.map(t => (t._2, t._1))).filter(_._1 < 15548).filter(t => t._1 <= visitedStates(t._2))
      if (System.currentTimeMillis() > time) {
        println(s"${new Date()} fringe size: ${fringe.size} allStatesSize: ${visitedStates.size} current best score: ${first._1}")
        time = System.currentTimeMillis() + 30000;
      }

    }
  }

}
