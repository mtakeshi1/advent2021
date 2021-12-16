package week3

import scala.annotation.tailrec

object Day16 {

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

  @tailrec
  def pack(in: List[Binary], acc: Long = 0): Long = {
    in match {
      case Nil => acc
      case head :: next => pack(next, (acc << 1) + head.mask)
      //(head.mask << 1) + pack(next)
    }
  }


  trait Packet {
    def version: Int

    def packetType: Int

    def versionSum: Int

    def eval: Long

    def evalGuard: Long = {
      val l = eval;
      assert(l >= 0)
      l
    }

    def preffix(spaces: Int) = {
      "-" * spaces
    }

    def tree(spaces: Int = 0): String = s"${preffix(spaces)} $eval"

    def exp: String = eval.toString

  }

  case class LiteralPacket(version: Int, packetType: Int, literal: Long) extends Packet {
    override def versionSum: Int = version

    override def eval: Long = literal

    override def exp: String = literal+"L"

    override def toString: String = String.valueOf(literal)

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
    override def evalGuard: Long = {
      val l:Long = packetType match {
        case 0 => subPackets.map(_.evalGuard).sum
        case 1 => subPackets.map(_.evalGuard).product
        case 2 => subPackets.map(_.evalGuard).min
        case 3 => subPackets.map(_.evalGuard).max
        case 5 =>
          assert(subPackets.size == 2)
          if (subPackets(0).evalGuard > subPackets(1).evalGuard) 1 else 0
        case 6 =>
          assert(subPackets.size == 2)
          if (subPackets(0).evalGuard < subPackets(1).evalGuard) 1 else 0
        case 7 =>
          assert(subPackets.size == 2)
          if (subPackets(0).evalGuard == subPackets(1).evalGuard) 1 else 0
        case _ => sys.error(s"unknown type: $packetType")
      }
      assert(l >= 0)
      l
    }

    override def tree(spaces: Int): String = {
      preffix(spaces) + (packetType match {
        case 0 => "SUM\n" + subPackets.map(_.tree(spaces + 1)).mkString("\n")
        case 1 => "PRODUCT\n" + subPackets.map(_.tree(spaces + 1)).mkString("\n")
        case 2 => "MIN\n" + subPackets.map(_.tree(spaces + 1)).mkString("\n")
        case 3 => "MAX\n" + subPackets.map(_.tree(spaces + 1)).mkString("\n")
        case 5 => ">\n" + subPackets.map(_.tree(spaces + 1)).mkString("\n")
        case 6 => "<\n" + subPackets.map(_.tree(spaces + 1)).mkString("\n")
        case 7 => "==\n" + subPackets.map(_.tree(spaces + 1)).mkString("\n")
        case _ => sys.error(s"unknown type: $packetType")
      })
    }

    override def exp: String = {
      (packetType match {
        case 0 => subPackets.map(_.exp).mkString("List(", ",", ").sum")
        case 1 => subPackets.map(_.exp).mkString("List(", ",", ").product")
        case 2 => subPackets.map(_.exp).mkString("List(", ",", ").min")
        case 3 => subPackets.map(_.exp).mkString("List(", ",", ").max")
        case 5 => s"if( ${subPackets(0).exp} > ${subPackets(1).exp} ) 1L else 0L"
        case 6 => s"if( ${subPackets(0).exp} < ${subPackets(1).exp} ) 1L else 0L"
        case 7 => s"if( ${subPackets(0).exp} == ${subPackets(1).exp} ) 1L else 0L"
        case _ => sys.error(s"unknown type: $packetType")
      })
    }

    override def toString: String = {

      packetType match {
        case 0 => subPackets.map(_.toString).mkString("SUM ( ", " , ", " ) ")
        case 1 => subPackets.map(_.toString).mkString("PRODUCT ( ", " , ", " ) ")
        case 2 => subPackets.map(_.toString).mkString("MIN ( ", " , ", " ) ")
        case 3 => subPackets.map(_.toString).mkString("MAX ( ", " , ", " ) ")
        case 5 =>
          assert(subPackets.size == 2)
          s"${subPackets(0)} > ${subPackets(1)}"
        case 6 =>
          assert(subPackets.size == 2)
          s"${subPackets(0)} < ${subPackets(1)}"
        case 7 =>
          assert(subPackets.size == 2)
          s"${subPackets(0)} == ${subPackets(1)}"

      }
    }

  }

  def parseLiteralGroup(in: List[Binary], acc: Long = 0): (Long, List[Binary]) = {
    val group = in.take(5)
    val num = pack(group.tail)
    if (group.head == One) {
      val (l, rem) = parseLiteralGroup(in.drop(5), acc << 4 | num)
      (l, rem)
    }
    else (acc << 4 | num, in.drop(5))
  }

  def parseLiteralPacket(v: Int, t: Int, in: List[Binary]): (LiteralPacket, List[Binary]) = {
    val (literal, rem) = parseLiteralGroup(in)
    (LiteralPacket(v, t, literal), rem)
  }

  def parseOperatorPacket(v: Int, t: Int, in: List[Binary]): (OpPacket, List[Binary]) = {
    val lenType = in.head
    if(lenType == Zero) {
//      val lenBits = if (lenType == Zero) 15 else 11
      val length = pack(in.tail.take(15)).toInt
      val content = in.drop(16).take(length)
      val rem = in.drop(16 + length)
      val subPackets = parsePackets(content)
      (OpPacket(v, t, subPackets), rem)
    } else {
      val numSubs = pack(in.tail.take(11)).toInt
      var next = in.drop(12)
      var subPackets: List[Packet] = List()
      val (subPack, rest) = 0.until(numSubs).foldLeft((subPackets, next))((tuple, _) => {
        val (p, r) = parsePacket(tuple._2)
        (p :: tuple._1, r)
      })
      (OpPacket(v, t, subPack.reverse), rest)
    }
  }


  def parsePacket(in: List[Binary]): (Packet, List[Binary]) = {
    val version = pack(in.take(3)).toInt
    val t = pack(in.drop(3).take(3)).toInt
    if (t == 4) {
      parseLiteralPacket(version, t, in.drop(6))
    } else {
      parseOperatorPacket(version, t, in.drop(6))
    }
  }

  def parsePackets(in: List[Binary]): List[Packet] = {
    in match {
      case Nil => List()
      case ::(head, next) =>
        val (p, rem) = parsePacket(in)
        p :: parsePackets(rem)
    }
  }

  def parse(input: String) = {
    input.toList.flatMap(charToNum)
  }

  def main(args: Array[String]): Unit = {
    println(parsePacket(parse("D2FE28"))._1.exp)
//    println(parsePacket(parse("C200B40A82"))._1.tree())
//    println(parsePacket(parse("04005AC33890"))._1.tree())
//    println(parsePacket(parse("880086C3E88112"))._1.tree())
//    println(parsePacket(parse("CE00C43D881120"))._1.tree())
//    println(parsePacket(parse("D8005AC2A8F0"))._1.tree())
//    println(parsePacket(parse("F600BC2D8F"))._1.tree())
//    println(parsePacket(parse("9C005AC2F8F0"))._1.exp)
//    println(parsePacket(parse("3600888023024c01150044c0118330a440118330e44011833085c0118522008c29870"))._1.exp)
    val input = "220D6448300428021F9EFE668D3F5FD6025165C00C602FC980B45002A40400B402548808A310028400C001B5CC00B10029C0096011C0003C55003C0028270025400C1002E4F19099F7600142C801098CD0761290021B19627C1D3007E33C4A8A640143CE85CB9D49144C134927100823275CC28D9C01234BD21F8144A6F90D1B2804F39B972B13D9D60939384FE29BA3B8803535E8DF04F33BC4AFCAFC9E4EE32600C4E2F4896CE079802D4012148DF5ACB9C8DF5ACB9CD821007874014B4ECE1A8FEF9D1BCC72A293A0E801C7C9CA36A5A9D6396F8FCC52D18E91E77DD9EB16649AA9EC9DA4F4600ACE7F90DFA30BA160066A200FC448EB05C401B8291F22A2002051D247856600949C3C73A009C8F0CA7FBCCF77F88B0000B905A3C1802B3F7990E8029375AC7DDE2DCA20C2C1004E4BE9F392D0E90073D31634C0090667FF8D9E667FF8D9F0C01693F8FE8024000844688FF0900010D8EB0923A9802903F80357100663DC2987C0008744F8B5138803739EB67223C00E4CC74BA46B0AD42C001DE8392C0B0DE4E8F660095006AA200EC198671A00010E87F08E184FCD7840289C1995749197295AC265B2BFC76811381880193C8EE36C324F95CA69C26D92364B66779D63EA071008C360098002191A637C7310062224108C3263A600A49334C19100A1A000864728BF0980010E8571EE188803D19A294477008A595A53BC841526BE313D6F88CE7E16A7AC60401A9E80273728D2CC53728D2CCD2AA2600A466A007CE680E5E79EFEB07360041A6B20D0F4C021982C966D9810993B9E9F3B1C7970C00B9577300526F52FCAB3DF87EC01296AFBC1F3BC9A6200109309240156CC41B38015796EABCB7540804B7C00B926BD6AC36B1338C4717E7D7A76378C85D8043F947C966593FD2BBBCB27710E57FDF6A686E00EC229B4C9247300528029393EC3BAA32C9F61DD51925AD9AB2B001F72B2EE464C0139580D680232FA129668"
    println(parsePacket(parse(input))._1.evalGuard)
  }

}
