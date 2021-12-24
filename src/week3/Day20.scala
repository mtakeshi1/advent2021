package week3

import scala.annotation.tailrec

object Day20 {

  trait Binary {
    def mask: Int
  }

  object Zero extends Binary {
    override def toString: String = "."

    def mask = 0
  }

  object One extends Binary {
    override def toString: String = "#"

    def mask = 1
  }

  @tailrec
  def binaryToInt(seq: List[Binary], acc: Int = 0): Int = {
    assert(seq.size < 32)
    seq match {
      case Nil => acc
      case head :: rest => binaryToInt(rest, (acc << 1) | head.mask)
    }
  }

  case class V2(x: Int, y: Int)

  case class Image(pixels: Map[V2, Binary], dimensions: V2, oob: Binary = Zero) {
    def get(v2: V2): Binary = pixels.getOrElse(v2, oob)

    def pixelBinary(v2: V2): Int = {
      val value: Seq[Binary] = for {
        y <- -1.to(1)
        x <- -1.to(1)
        v = V2(x + v2.x, y + v2.y)
      } yield get(v)
      binaryToInt(value.toList)
    }

    final def enhance(pixelMap: Array[Binary]): Image = {
      val newDim: V2 = V2(this.dimensions.x + 2, this.dimensions.y + 2)

      val map: Map[V2, Binary] = (for {
        y <- -1.to(dimensions.y)
        x <- -1.to(dimensions.x)
        encoded = pixelBinary(V2(x, y))
      } yield V2(x + 1, y + 1) -> pixelMap(encoded)).toMap
      val newOOB = pixelMap(
        binaryToInt(0.until(9).map(_ => oob).toList)
      )
      Image(map, newDim, newOOB)
    }

    @tailrec
    final def enhance(n: Int, pixelMap: Array[Binary]): Image = {
      if (n == 0) this
      else enhance(pixelMap).enhance(n - 1, pixelMap)
    }

    override def toString: String = {
      0.until(dimensions.y).map(y => 0.until(dimensions.x).map(x => get(V2(x, y)).toString).mkString("")).mkString("\n")
    }
  }

  def parse(file: String): (Array[Binary], Image) = {
    val lines = scala.io.Source.fromFile(file).getLines().map(_.trim)
    val pixelMap: Array[Binary] = lines.next().map(c => if (c == '.') Zero else One).toArray
    assert(lines.next().isBlank)

    def parseLine(line: String, offset: Int): Seq[(V2, Binary)] = {
      line.map(c => if (c == '.') Zero else One).zipWithIndex.map(t => V2(t._2, offset) -> t._1)
    }

    val map: Map[V2, Binary] = lines.zipWithIndex.flatMap(l => parseLine(l._1, l._2)).toMap
    val maxX = 1 + map.keys.map(_.x).max
    val maxY = 1 + map.keys.map(_.y).max
    (pixelMap, Image(map, V2(maxX, maxY)))
  }

  def main(args: Array[String]): Unit = {
    val (pixelMap, image) = parse("week3/input20.txt")
    //    assert(image.dimensions == V2(5, 5))
    //    assert(image.pixelBinary(V2(2, 2)) == 34)
    //    assert(pixelMap(34) == One)
    //    println(image)
    //    println(s"---------------------------- ${image.pixels.values.count(_ == One)}")
    //    val applyOne = image.enhance(pixelMap)
    //    println(applyOne)
    //    println(s"---------------------------- ${applyOne.pixels.values.count(_ == One)}")
    //    val applyTwo = applyOne.enhance(pixelMap)
    //    println(applyTwo)
    //    println(s"---------------------------- ${applyTwo.pixels.values.count(_ == One)}")
    val afterAll = image.enhance(50, pixelMap)
    println(afterAll.pixels.values.count(_ == One))
  }

}
