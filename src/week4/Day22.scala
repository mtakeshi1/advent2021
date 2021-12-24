package week4

import week4.Day22.Cuboid.glueRecursive

import java.util.Date
import scala.annotation.tailrec
import scala.io.Source

object Day22 {

  trait Reactor {
    def turnOn: Reactor = ReactorOn

    def turnOff: Reactor = ReactorOff

    def isOn: Boolean = this == ReactorOn
  }

  case object ReactorOn extends Reactor

  case object ReactorOff extends Reactor

  case class V3(x: Int, y: Int, z: Int) {
    def ge3(other: V3) = this.x >= other.x && this.y >= other.y && this.z >= other.z

    def gt3(other: V3) = this.x > other.x && this.y > other.y && this.z > other.z

    def le3(other: V3) = this.x <= other.x && this.y <= other.y && this.z <= other.z

    def lt3(other: V3) = this.x < other.x && this.y < other.y && this.z < other.z
  }

  case class AllReactors(state: Map[V3, Reactor] = Map().withDefaultValue(ReactorOff)) {
    val offset = 50

    def inRange(t: Int): Boolean = (-offset).to(offset).contains(t)

    def fix(range: Range) = Math.max(-50, range.min).to(Math.min(50, range.max))

    def turnOn(xr: Range, yr: Range, zr: Range) = {
      val r = for {
        x <- fix(xr)
        y <- fix(yr)
        z <- fix(zr)
      } yield (V3(x + offset, y + offset, z + offset) -> ReactorOn)
      AllReactors(r.toMap ++ state)
    }

    def turnOff(xr: Range, yr: Range, zr: Range) = {
      val r = for {
        x <- fix(xr)
        y <- fix(yr)
        z <- fix(zr)
      } yield V3(x + offset, y + offset, z + offset)
      AllReactors(state.removedAll(r))
    }
  }

  case class Cuboid(min: V3, max: V3) {
    val volume: Long = (max.x.toLong - min.x.toLong) * (max.y.toLong - min.y.toLong) * (max.z.toLong - min.z)

    def contains(other: Cuboid) = {
      val contained = other.min.ge3(this.min) && this.max.ge3(other.max)
      contained
    }

    def intersection(other: Cuboid): Option[Cuboid] = {
      if (this.max.gt3(other.min) && this.min.lt3(other.max)) {
        Some(Cuboid(
          V3(Math.max(this.min.x, other.min.x), Math.max(this.min.y, other.min.y), Math.max(this.min.z, other.min.z)),
          V3(Math.min(this.max.x, other.max.x), Math.min(this.max.y, other.max.y), Math.min(this.max.z, other.max.z))))
      } else None
    }

    def intersects(other: Cuboid) = intersection(other).isDefined

    def calculateDiff(other: Cuboid): Seq[Cuboid] = {
      if (this.contains(other)) List()
      else {
        val xs = List(this.min.x, this.max.x, other.min.x, other.max.x).sorted
        val ys = List(this.min.y, this.max.y, other.min.y, other.max.y).sorted
        val zs = List(this.min.z, this.max.z, other.min.z, other.max.z).sorted
        val xr = xs.zip(xs.tail)
        val yr = ys.zip(ys.tail)
        val zr = zs.zip(zs.tail)
        glueRecursive(for {
          (xmin, xmax) <- xr
          (ymin, ymax) <- yr
          (zmin, zmax) <- zr
          candidate = Cuboid(V3(xmin, ymin, zmin), V3(xmax, ymax, zmax))
          if !this.contains(candidate) && other.contains(candidate)
          //          _ = println(s"$candidate")
        } yield candidate)
      }
    }

    def calculateDiff2(other: Cuboid): Seq[Cuboid] = {
      if (this.contains(other)) List()
      else {
        var r: List[Cuboid] = List()
        var remaining = other;

        if (remaining.min.x < this.min.x) {
          r = Cuboid(remaining.min, remaining.max.copy(x = this.min.x)) :: r
          remaining = Cuboid(remaining.min.copy(x = this.min.x), remaining.max)
        }

        if (remaining.max.x > this.max.x) {
          r = Cuboid(remaining.min.copy(x = this.max.x), remaining.max) :: r
          remaining = Cuboid(remaining.min, remaining.max.copy(x = this.max.x))
        }

        if (remaining.min.y < this.min.y) {
          r = Cuboid(remaining.min, remaining.max.copy(y = this.min.y)) :: r
          remaining = Cuboid(remaining.min.copy(y = this.min.y), remaining.max)
        }

        if (remaining.max.y > this.max.y) {
          r = Cuboid(remaining.min.copy(y = this.max.y), remaining.max) :: r
          remaining = Cuboid(remaining.min, remaining.max.copy(y = this.max.y))
        }

        if (remaining.min.z < this.min.z) {
          r = Cuboid(remaining.min, remaining.max.copy(z = this.min.z)) :: r
          remaining = Cuboid(remaining.min.copy(z = this.min.z), remaining.max)
        }

        if (remaining.max.z > this.max.z) {
          r = Cuboid(remaining.min.copy(z = this.max.z), remaining.max) :: r
        }
        r
      }
    }

    def minus(other: Cuboid): Seq[Cuboid] = {
      other.calculateDiff2(this)
    }

  }


  object Cuboid {
    def canGlueX(l: Cuboid, r: Cuboid): Boolean = l.min.y == r.min.y && l.max.y == r.max.y && l.min.z == r.min.z && l.max.z == r.max.z

    def canGlueY(l: Cuboid, r: Cuboid): Boolean = l.min.x == r.min.x && l.max.x == r.max.x && l.min.z == r.min.z && l.max.z == r.max.z

    def canGlueZ(l: Cuboid, r: Cuboid): Boolean = l.min.y == r.min.y && l.max.y == r.max.y && l.min.x == r.min.x && l.max.x == r.max.x

    def canGlue(l: Cuboid, r: Cuboid): Boolean = canGlueX(l, r) || canGlueY(l, r) || canGlueZ(l, r)

    def glue(l: Cuboid, r: Cuboid): Cuboid = Cuboid(V3(Math.min(l.min.x, r.min.x), Math.min(l.min.y, r.min.y), Math.min(l.min.z, r.min.z)),
      V3(Math.max(l.max.x, r.max.x), Math.max(l.max.y, r.max.y), Math.max(l.max.z, r.max.z)))

    def glueRecursive(in: List[Cuboid]): List[Cuboid] = {
      in match {
        case Nil => List()
        case a :: rest => {
          val maybeGlue = rest.find(Cuboid.canGlue(a, _))
          if (maybeGlue.isDefined) {
            val m = rest.filter(_ != maybeGlue.get)
            glueRecursive(Cuboid.glue(a, maybeGlue.get) :: m)
          } else {
            a :: glueRecursive(rest)
          }
        }
      }
    }

  }

  def addReduce(currentCubes: List[Cuboid], next: Cuboid): List[Cuboid] = {
    //    if(currentCubes.isEmpty) List(next)
    //    else {
    //      currentCubes.foldLeft(List(next))((list, existing) => {
    //
    //        list
    //      })
    //      List()
    currentCubes match {
      case Nil => List(next)
      case ::(head, tail) => {
        val partial = head.calculateDiff2(next)
        partial.flatMap(t => addReduce(tail, t)).toList ::: currentCubes
      }
    }
  }

  def execute(input: List[(Cuboid, Boolean)]) = {
    var result: List[Cuboid] = List()
    input.foreach(in => {
      if (in._2) {
        result = glueRecursive(addReduce(result, in._1))
      } else {
        result = glueRecursive(result.flatMap(_.minus(in._1)))
      }
      println(s"${new Date()} result: ${result.size}")
    })

  }


  def parseRange(r: String): Range = {
    val rr = r.split("[.]+")
    rr(0).toInt.to(rr(1).toInt)
  }

  def parseLine(line: String): AllReactors => AllReactors = {
    val on = line.startsWith("on ")
    val parts: Array[Range] = line.substring(3).split(",").map(_.split("=")(1)).map(parseRange)
    assert(parts.size == 3)
    (r: AllReactors) => {
      println(s"applying $line")
      if (on) r.turnOn(parts(0), parts(1), parts(2)) else r.turnOff(parts(0), parts(1), parts(2))
    }
  }


  def parseLineB(line: String): (Cuboid, Boolean) = {
    val on = line.startsWith("on ")
    val parts: Array[Range] = line.substring(3).split(",").map(_.split("=")(1)).map(parseRange)
    assert(parts.size == 3)
    (Cuboid(V3(parts(0).min, parts(1).min, parts(2).min), V3(parts(0).max, parts(1).max, parts(2).max)), on)
  }


  def parseFile(file: String) = {
    Source.fromFile(file).getLines().map(_.trim).filterNot(_.isEmpty).map(parseLine).foldLeft(AllReactors())((r, f) => f(r))
  }

  def parseFileB(file: String): Seq[(Cuboid, Boolean)] = {
    Source.fromFile(file).getLines().map(_.trim).filterNot(_.isEmpty).map(parseLineB).toList.dropWhile(!_._2)
  }

  def cuboidTests = {

    val c1 = Cuboid(V3(0, 0, 0), V3(1, 1, 1))
    val c2 = Cuboid(V3(5, 5, 5), V3(10, 10, 10))

    val c0 = Cuboid(V3(0, 0, 0), V3(10, 10, 10))
    val c3 = Cuboid(V3(5, 5, 5), V3(12, 12, 12))
    val value = c0.calculateDiff2(c3)
    value.foreach(println)
    value.filter(_.intersects(c0)).foreach(println)
    assert(value.forall(c => !c.intersects(c0)))

  }

  def main(args: Array[String]): Unit = {
    val smallSample =
      """
        |on x=10..12,y=10..12,z=10..12
        |on x=11..13,y=11..13,z=11..13
        |off x=9..11,y=9..11,z=9..11
        |on x=10..10,y=10..10,z=10..10
        |""".stripMargin

    //    val reactors = smallSample.split("\n").filterNot(_.isEmpty).map(parseLine).foldLeft(AllReactors())((r, f) => f(r))
    //    assert(reactors.state.size == 39)
    //    val size = parseFile("week4/sample22.txt").state.size
    //    assert(size == 590784)
    //    println(parseFile("week4/input22.txt").state.size)
    cuboidTests
    val test = parseFileB("week4/input22.txt")
    var onlyOns = test.takeWhile(_ => true).map(_._1)
    execute(test.toList)
    //    println(parseFileB("week4/sample22b.txt"))
  }

}
