package week3

import java.util.Date
import scala.annotation.tailrec

object Day19 {


  trait Transformation {
    def transform(v: V3): V3

    def compose(other: Transformation): Transformation = CompositeTranformation(this, other)
  }

  case class CompositeTranformation(l: Transformation, r: Transformation) extends Transformation {
    override def transform(v: V3): V3 = r.transform(l.transform(v))

    override def toString: String = s"$l $r"
  }

  case class RotateX(n: Int) extends Transformation {
    override def transform(v: V3): V3 = v.rotateX(n)

    override def toString: String = if (n == 0) "" else s"X($n)"
  }

  case class RotateY(n: Int) extends Transformation {
    override def transform(v: V3): V3 = v.rotateY(n)

    override def toString: String = if (n == 0) "" else s"Y($n)"
  }

  case class RotateZ(n: Int) extends Transformation {
    override def transform(v: V3): V3 = v.rotateZ(n)

    override def toString: String = if (n == 0) "" else s"Z($n)"
  }

  def allTransformations: Seq[Transformation] = for {
    x <- 0.until(4)
    y <- 0.until(4)
    z <- 0.until(4)
  } yield RotateX(x).compose(RotateY(y)).compose(RotateZ(z))

  val distinctTransformations = allTransformations.distinctBy(_.transform(V3(1, 2, 3)))

  case class V3(x: Int, y: Int, z: Int) {
    def plus(other: V3): V3 = V3(this.x + other.x, this.y + other.y, this.z + other.z)

    def minus(other: V3) = V3(this.x - other.x, this.y - other.y, this.z - other.z)

    def invert = V3(-x, -y, -z)

    def plus(scalar: Int): V3 = V3(this.x + scalar, this.y + scalar, this.z + scalar)

    def multiply(other: V3) = V3(this.x * other.x, this.y * other.y, this.z * other.z)

    def rotateX: V3 = V3(x, -z, y)

    @tailrec final def rotateX(n: Int): V3 = if (n == 0) this else this.rotateX.rotateX(n - 1)

    def rotateY: V3 = V3(z, y, -x)

    @tailrec final def rotateY(n: Int): V3 = if (n == 0) this else this.rotateY.rotateY(n - 1)

    def rotateZ: V3 = V3(-y, x, z)

    @tailrec final def rotateZ(n: Int): V3 = if (n == 0) this else this.rotateZ.rotateZ(n - 1)

    def manhantanDistance(o: V3): Int = (this.x - o.x).abs + (this.y - o.y).abs + (this.z + o.z).abs

  }

  case class Scanner(name: String, beacons: Set[V3]) {
    def rotations(): Seq[Scanner] = distinctTransformations.map(t => Scanner(name + t, beacons.map(t.transform)))

    def overlapWith(other: Scanner) = {
      (for {
        myBeacon <- beacons.to(LazyList)
        otherBeacon <- other.beacons
        diff = otherBeacon.minus(myBeacon)
        otherPOV = other.beacons.map(_.minus(diff))
        //        _ = println(s"trying $diff")
        if beacons.intersect(otherPOV).size >= 12
        //        _ = println(s"found $diff")
      } yield diff).headOption
    }

    def findOverlapFor(other: Scanner): Option[Scanner] = {
      other.rotations().to(LazyList).map(o => (o, this.overlapWith(o))).find(!_._2.isEmpty).map(t => {
        t._1.minus(t._2.get)
      })
    }

    def findOverlapKeepCenter(other: Scanner): Option[(Scanner, V3)] = {
      other.rotations().to(LazyList).map(o => (o, this.overlapWith(o))).find(!_._2.isEmpty).map(t => {
        (t._1.minus(t._2.get), t._2.get)
      })
    }

    def minus(v: V3) = Scanner(this.name, beacons.map(_.minus(v)))

    def merge(other: Scanner) = Scanner(s"${this.name}_${other.name}", this.beacons ++ other.beacons)
  }

  def parseScanner(lines: Iterator[String]): Scanner = {
    val header = lines.next()
    assert(header.startsWith("--- scanner"))
    val beacons = lines.takeWhile(_.matches("^-?[0-9].+")).map(_.split(",")).map(array => V3(array(0).toInt, array(1).toInt, array(2).toInt)).toSet
    Scanner(header.replaceAll("-", "").trim, beacons)
  }

  def parse(file: String): Seq[Scanner] = {
    val lines = scala.io.Source.fromFile(file).getLines().map(_.trim)
    var scanners: List[Scanner] = List()
    while (lines.hasNext) {
      scanners = parseScanner(lines) :: scanners
      //      val emptyLine = lines.next()
      //      assert(emptyLine == "")
    }
    scanners.reverse
  }

  def findOverlapOnGroup(group: List[Scanner], lookup: Scanner): Option[Scanner] = {
    group.to(LazyList).map(_.findOverlapFor(lookup)).find(_.isDefined).getOrElse(None)
  }

  def reduceToOne(input: Seq[Scanner]): Scanner = {
    var result = input.head
    var toReduce = input.tail;
    while (!toReduce.isEmpty) {
      println(s"${new Date()}: to reduce: ${toReduce.size} - beacons: ${result.beacons.size}")
      val option: Option[(Scanner, Scanner)] = toReduce.to(LazyList).map(candidate => {
        (candidate, result.findOverlapFor(candidate))
      }).filter(_._2.isDefined).map(t => (t._1, t._2.get)).headOption
      option match {
        case Some((original, rotated)) => {
          toReduce = toReduce.filter(_ != original)
          result = result.merge(rotated)
        }
        case None => sys.error("?")
      }
    }
    result
  }


  def reduceToOneB(input: Seq[Scanner]): List[V3] = {
    var result = input.head
    var centers = List(V3(0, 0, 0))
    var toReduce = input.tail;
    while (!toReduce.isEmpty) {
      println(s"${new Date()}: to reduce: ${toReduce.size} - beacons: ${result.beacons.size}")
      val option: Option[(Scanner, (Scanner, V3))] = toReduce.to(LazyList).map(candidate => {
        (candidate, result.findOverlapKeepCenter(candidate))
      }).filter(_._2.isDefined).map(t => (t._1, t._2.get)).headOption
      option match {
        case Some((original, (rotated, center))) => {
          toReduce = toReduce.filter(_ != original)
          result = result.merge(rotated)
          centers = center :: centers
        }
        case None => sys.error("?")
      }
    }
    centers
  }

  def main(args: Array[String]): Unit = {
    //    distinctTransformations.foreach(println)
    val scanners = parse("week3/input19.txt")
    //    assert(scanners.size == 5)
    //    assert(scanners.head.name == "scanner 0")
    //    assert(scanners.head.beacons.size == 25)
    //    val scanner0 = scanners.head
    //    val scanner1 = scanners(1)
    //    scanner1.rotations().find(s => !scanner0.overlapWith(s).isEmpty).get.beacons.foreach(println)
    //    scanner0.beacons.foreach(println)
    //    scanner0.findOverlapFor(scanner1).get.beacons.foreach(println)
    //    println(reduceToOne(scanners).beacons.size)
    val centers = reduceToOneB(scanners)
    var maxDistance = 0;
    val distances = for {
      i <- 0.until(centers.size)
      j <- (i + 1).until(centers.size)
    } yield centers(i).manhantanDistance(centers(j))
    println(distances.max)
  }


}
