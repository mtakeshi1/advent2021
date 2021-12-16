package week2

object Day10 {

  case class ChunkDelim(open: Char, close: Char, score1: Int, score2: Int)

  val parents = ChunkDelim('(', ')', 3, 1)
  val square = ChunkDelim('[', ']', 57, 2)
  val curly = ChunkDelim('{', '}', 1197, 3)
  val angle = ChunkDelim('<', '>', 25137, 4)

  val delim = List(parents, square, curly, angle)

  def isOpenChunk(c: Char) = delim.find(chunk => chunk.open == c).isDefined

  def isCloseChunk(c: Char) = delim.find(chunk => chunk.close == c).isDefined

  def isMatch(open: Char, close: Char): Boolean = {
    val s = delim.find(chunk => chunk.open == open && chunk.close == close)
    s.isDefined
  }

  def scoreFor(c: Char) = {
    val s = delim.find(_.close == c).map(_.score1)
    if (s.isEmpty) sys.error(s"request close char: $c")
    s.get
  }

  def calculateIllegal(stack: List[Char], input: List[Char]): Int = {
    input match {
      case List() => 0
      case head :: tail =>
        if (isOpenChunk(head))
          calculateIllegal(head :: stack, tail) + 0
        else if (isMatch(stack.head, head)) {
          calculateIllegal(stack.tail, tail) + 0
        }
        else scoreFor(head)
    }
  }

  def calculateMissingChars(stack: List[Char], input: List[Char]): List[Char] = {
    input match {
      case List() => stack
      case head :: tail =>
        if (isOpenChunk(head))
          calculateMissingChars(head :: stack, tail)
        else if (isMatch(stack.head, head)) {
          calculateMissingChars(stack.tail, tail)
        }
        else List()
    }
  }

  def calcPart2(stack: List[Char], score: Long = 0): Long = {
    stack match {
      case Nil => score
      case ::(head, next) => calcPart2(next, 5 * score + delim.find(_.open == head).map(_.score2).get)
    }
  }

  def part1(file: String): Long = {
    //    println(scala.io.Source.fromFile(file).getLines().map(_.trim).map(line => s"$line => ${calculateIllegal(List(), line.toList)}").mkString("\n"))
    scala.io.Source.fromFile(file).getLines().map(_.trim).map(line => calculateIllegal(List(), line.toList)).sum
  }

  def part2line(line: String): Long = {
    val rem = calculateMissingChars(List(), line.toList)
    calcPart2(rem)
  }

  def part2(file: String): Long = {
    val input = scala.io.Source.fromFile(file).getLines().map(_.trim).map(part2line).filter(_ != 0).toList.sorted
    println(input)
    input(input.size / 2)
  }

  def main(args: Array[String]): Unit = {
    //    println(calculateIllegal(List(), input = "[({(<(())[]>[[{[]{<()<>>".toList))
    //    println(part1("sample10.txt"))
    //    println(part1("input10.txt"))
    println(part2line("[({(<(())[]>[[{[]{<()<>>"))
    println(part2("sample10.txt"))
    println(part2("input10.txt"))
  }


}
