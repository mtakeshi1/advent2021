package week3

import scala.annotation.tailrec

object Day16State {
  case class ListState[A, B](function: List[A] => (List[A], B)) {
    def eval(state: List[A]): B = function(state)._2

    def map[C](f: B => C): ListState[A, C] = {
      def inner(input: List[A]): (List[A], C) = {
        val b: (List[A], B) = function(input)
        (b._1, f(b._2))
      }
      ListState[A, C](inner)
    }

    def flatMap[C](f: B => ListState[A, C]): ListState[A, C] = {
      def evall(state: List[A]): (List[A], C) = {
        val r: (List[A], B) = function(state)
        val insideState: ListState[A, C] = f(r._2)
        insideState.function(r._1)
      }
      new ListState[A, C](evall)
    }
  }

  trait Binary {
    def mask: Int
  }

  object Zero extends Binary {
    override def toString: String = "0"
    def mask = 0
  }

  object One extends Binary {
    override def toString: String = "1"
    def mask = 1
  }

  def toBinary(v: Int) = if (v == 0) Zero else One

  def charToNum(char: Char): List[Binary] = {
    val asInt = if (char >= '0' && char <= '9') char - '0' else 10 + char - 'A'
    List(toBinary(asInt & 8), toBinary(asInt & 4), toBinary(asInt & 2), toBinary(asInt & 1))
  }

  trait Packet {
    def version: Int
    def packetType: Int
    def versionSum: Int
    def eval: Long

  }

  case class LiteralPacket(version: Int, packetType: Int, literal: Long) extends Packet {
    override def versionSum: Int = version
    override def eval: Long = literal
  }

  case class OpPacket(version: Int, packetType: Int, subPackets: List[Packet]) extends Packet {
    override def versionSum: Int = version + subPackets.map(_.versionSum).sum

    override def eval: Long = {
      packetType match {
        case 0 => subPackets.map(_.eval).sum
        case 1 => subPackets.map(_.eval).product
        case 2 => subPackets.map(_.eval).min
        case 3 => subPackets.map(_.eval).max
        case 5 =>
          assert(subPackets.size == 2)
          if(subPackets(0).eval > subPackets(1).eval) 1 else 0
        case 6 =>
          assert(subPackets.size == 2)
          if(subPackets(0).eval < subPackets(1).eval) 1 else 0
        case 7 =>
          assert(subPackets.size == 2)
          if(subPackets(0).eval == subPackets(1).eval) 1 else 0
        case _ =>
          sys.error(s"???? $packetType")
      }
    }

  }


  @tailrec
  def pack(in: List[Binary], acc: Long = 0): Long = {
    in match {
      case Nil => acc
      case head :: next => pack(next, (acc << 1) + head.mask)
    }
  }

  def take(num: Int) = ListState[Int, Int](l => (l.drop(num), l.take(num).size))

  def removeFirst = ListState[Binary, Binary](l => (l.tail, l.head))

  def nibs(num: Int) = ListState[Binary, Long](l => {
    val (pre, rest) = l.splitAt(num)
    assert(pre.size == num)
    (rest, pack(pre))
  })

  @tailrec
  final def parseLiteral(l: List[Binary], acc: Long = 0): (List[Binary], Long) = {
    val (pre, rem) = l.splitAt(5)
    assert(pre.size == 5)
    val num = pack(pre.tail)
    if(pre.head == One) {
      parseLiteral(rem, num << 4 | acc)
    } else (rem, num)
  }

  def parsePacket() = {
    for {
      version <- nibs(3)
      pType <- nibs(3)
      literal <- ListState[Binary, Long](l => parseLiteral(l))
    } yield literal
  }

  def bla = {
    for {
      x <- take(10)
    } yield x
  }

  def main(args: Array[String]): Unit = {
    println(bla.function(List(1,2)))
  }


}
