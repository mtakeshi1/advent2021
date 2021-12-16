package week2

import scala.annotation.tailrec

object Day14 {

  case class Input(mappings: Map[(Char, Char), Char], sequence: List[Char]) {


    def step() = {
      @tailrec
      def stepR(in: List[Char] = sequence, acc: List[Char] = List()): List[Char] = {
        in match {
          case a :: b :: rest => stepR(b :: rest, mappings((a, b)) :: a :: acc)
          case a :: List() => (a :: acc).reverse
          case List() => acc.reverse
        }
      }

      Input(mappings, stepR())
    }

    def steps(i: Int): Input = if (i > 0) step().steps(i - 1) else this

    def pairs = {
      val ts = 0.until(sequence.size).zip(1.until(sequence.size)).map(t => (sequence(t._1), sequence(t._2)))
      var m: Map[(Char, Char), Long] = Map().withDefaultValue(0)
      ts.foreach(t => {
        m = m.updated(t, m(t) + 1)
      })
      m
    }

    def formatPairs = {
      pairs.toList.sortBy(t => t._1.toString()).map(t => s"${t._1._1}${t._1._2} -> ${t._2}").mkString("\n")
    }

  }

  case class InputB(mappings: Map[(Char, Char), Char], sequence: Array[Char]) {


    def step(): InputB = {
      val newSize = sequence.length - 1 + sequence.length
      val output = new Array[Char](newSize)
      0.until(sequence.size - 1).foreach(i => {
        output(2 * i) = sequence(i)
        output(2 * i + 1) = mappings((sequence(i), sequence(i + 1)))
      })
      output(newSize - 1) = sequence(sequence.size - 1)
      InputB(mappings, output)
    }

    def pairs = {
      val ts = 0.until(sequence.size).zip(1.until(sequence.size)).map(t => (sequence(t._1), sequence(t._2)))
      var m: Map[(Char, Char), Long] = Map().withDefaultValue(0)
      ts.foreach(t => {
        m = m.updated(t, m(t) + 1)
      })
      m
    }

    def numChars(): Map[Char, Long] = {
      val m: Map[Char, Long] = Map().withDefaultValue(0)
      sequence.foldLeft(m)((map, c) => map.updated(c, map(c) + 1))
    }

    def steps(i: Int): InputB = if (i > 0) step().steps(i - 1) else this

  }

  case class InputB2(mappings: Map[(Char, Char), Char], pairs: Map[(Char, Char), Long], borders: (Char, Char)) {
    def step() = {
      var nPairs: Map[(Char, Char), Long] = Map().withDefaultValue(0L)
      pairs.foreach(t => {
        val p = t._1
        val n = t._2
        val key = "" + p._1 + p._2
        val c = mappings(p)
        val left = (p._1, c)
        val right = (c, p._2)
        nPairs = nPairs.updated(left, n + nPairs(left))
        nPairs = nPairs.updated(right, n + nPairs(right))
      })
      InputB2(mappings, nPairs, borders)
    }

    def count(): Map[Char, Long] = {
      var c: Map[Char, Long] = Map(borders._1 -> 1L).withDefaultValue(0L)
      c = c.updated(borders._2, 1 + c(borders._2))
      pairs.foreach(p => {
        val (a, b) = p._1
        val n = p._2
        c = c.updated(a, c(a) + n)
        c = c.updated(b, c(b) + n)
      })
      c.map(t => (t._1, t._2 / 2))
    }

    def result = {
      val m = count();
      val max = m.maxBy(_._2)
      val min = m.minBy(_._2)
      max._2 - min._2
    }

    @tailrec
    final def steps(i: Int): InputB2 = if (i == 0) this else step().steps(i - 1)

    def formatPairs = {
      pairs.toList.sortBy(t => t._1.toString()).map(t => s"${t._1._1}${t._1._2} -> ${t._2}").mkString("\n")
    }

    def numChars(): Map[Char, Long] = count()

  }

  def parse(file: String): Input = {
    val lines = scala.io.Source.fromFile(file).getLines().map(_.trim)
    val sequence = lines.takeWhile(_.size > 0).next.toList
    val mappings = lines.map(l => l.split("->").map(_.trim)).filter(_.size == 2).map(ar => ((ar(0)(0), ar(0)(1)), ar(1)(0))).toMap

    Input(mappings, sequence)
  }

  def parseB(file: String): InputB = {
    val lines = scala.io.Source.fromFile(file).getLines().map(_.trim)
    val sequence = lines.takeWhile(_.size > 0).next.toList
    val mappings = lines.map(l => l.split("->").map(_.trim)).filter(_.size == 2).map(ar => ((ar(0)(0), ar(0)(1)), ar(1)(0))).toMap

    InputB(mappings, sequence.toArray)
  }

  def parseB2(file: String) = {
    val lines = scala.io.Source.fromFile(file).getLines().map(_.trim)
    val sequence = lines.takeWhile(_.size > 0).next.toArray
    val mappings = lines.map(l => l.split("->").map(_.trim)).filter(_.size == 2).map(ar => ((ar(0)(0), ar(0)(1)), ar(1)(0))).toMap
    val ts = 0.until(sequence.size).zip(1.until(sequence.size)).map(t => (sequence(t._1), sequence(t._2)))
    var m: Map[(Char, Char), Long] = Map().withDefaultValue(0)
    ts.foreach(t => {
      m = m.updated(t, m(t) + 1)
    })
    m
    InputB2(mappings, m, (sequence(0), sequence(sequence.length - 1)))
  }

  def r(s: List[Char]) = {
    var count: Map[Char, Long] = Map[Char, Long]().withDefaultValue(0L)
    s.foreach(c => {
      count = count.updated(c, 1L + count(c))
    })
    val c: List[(Char, Long)] = count.toList
    val min = c.minBy(_._2)._2
    val max = c.maxBy(_._2)._2
    max - min
  }

  def check(s: String) = {
    s.toList.distinct.map(c => (c, s.count(_ == c))).toMap
  }

  def compare(m0: Map[Char, Long], m1: Map[Char, Long]): Boolean = {
    if (m0.size != m1.size) {
      println(s"size mismatch: ${m0.size} and ${m1.size}")
      return false;
    }
    m0.keys.foreach { t => {
      if (m0(t) != m1(t)) {
        println(s"key mismatch ${t} -> ${m0(t)} and ${m1(t)}")
        return false
      }
    }
    }
    true;
  }

  def main(args: Array[String]): Unit = {
    val input = "input14.txt"
    var sequenceb2: InputB2 = parseB2(input)
    //    var sequence1: InputB = parseB(input)
    //    var i = 1;
    //    while (true) {
    //      sequence1 = sequence1.step()
    //      sequenceb2 = sequenceb2.step()
    //
    //      if (!compare(sequence1.numChars(), sequenceb2.numChars())) {
    //        sys.error(s"after $i steps, it messed up")
    //      } else {
    //        println(s"${new Date()}: $i -> ${sequenceb2.count().values.sum}")
    //      }
    //      i = i + 1
    //    }
    println(sequenceb2.steps(40).result)
    //    2.until(10).foreach{ i =>
    //      val s2b = sequenceb2.steps(i).pairs
    //      val sa = sequence1.steps(i).pairs
    //      if(!compare(s2b, sa)) {
    //        println(s"for index $i")
    //        println(sequence1.steps(i-1).sequence.mkString(""))
    //        println(sequence1.steps(i).sequence.mkString(""))
    //        System.exit(0)
    //      }
    //    }

  }

}
