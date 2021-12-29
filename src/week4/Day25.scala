package week4

import scala.annotation.tailrec

object Day25 {

  val right = '>'
  val down = 'v'
  val empty = '.'

  case class Board(board: Seq[Array[Char]]) {

    def getWrap(line: Int, col: Int): Char = {
      if (line < 0) getWrap(line + board.size, col)
      else if (col < 0) getWrap(line, col + board(0).size)
      else {
        val thisLine = board(line % board.size)
        thisLine(col % thisLine.size)
      }
    }

    def setWrap(line: Int, column: Int, c: Char) = {
      if (line < 0) getWrap(line + board.size, column)
      else if (column < 0) getWrap(line, column + board(0).size)
      else {
        val thisLine = board(line % board.size)
        thisLine(column % thisLine.size) = c
      }
    }

    @tailrec
    final def steps(n: Int): Board = if (n == 0) this else this.next.steps(n - 1)

    def next: Board = {
      val nextBoard = Board(this.board.map(arr => arr.map(_ => '.')))

      0.until(board.size).foreach(line => {
        0.until(board(line).size).foreach(column => {
          if (getWrap(line, column) == right) {
            if (getWrap(line, column + 1) == '.') nextBoard.setWrap(line, column + 1, right)
            else nextBoard.setWrap(line, column, right)
          }
        })
      })
      0.until(board.size).map(line => {
        0.until(board(line).size).map(column => {
          if (getWrap(line, column) == down) {
            if (getWrap(line + 1, column) != down && nextBoard.getWrap(line + 1, column) == '.') nextBoard.setWrap(line + 1, column, down)
            else nextBoard.setWrap(line, column, down)
          }
        })
      })
      nextBoard
    }

    override def toString: String = board.map(_.mkString("")).mkString("\n")
  }

  def parseBoard(lines: Iterator[String]) = {
    Board(lines.takeWhile(!_.isEmpty).map(_.toCharArray).toList)
  }

  def main(args: Array[String]): Unit = {
    val lines =
      """
        |v...>>.vv>
        |.vv>>.vv..
        |>>.>v>...v
        |>>v>>.>.v.
        |v>v.vv.v..
        |>.>>..v...
        |.vv..>.>v.
        |v.v..>>v.v
        |....v..v.>
        |""".stripMargin

    val board = parseBoard(lines.split("\n").filterNot(_.isEmpty).iterator)
    assert(
      """v...>>.vv>
        |.vv>>.vv..
        |>>.>v>...v
        |>>v>>.>.v.
        |v>v.vv.v..
        |>.>>..v...
        |.vv..>.>v.
        |v.v..>>v.v
        |....v..v.>""".stripMargin == board.toString)

    println(board.next.toString)

    assert(
      """....>.>v.>
        |v.v>.>v.v.
        |>v>>..>v..
        |>>v>v>.>.v
        |.>v.v...v.
        |v>>.>vvv..
        |..v...>>..
        |vv...>>vv.
        |>.v.v..v.v""".stripMargin == board.next.toString)

    assert(
      """..>>v>vv..
        |..v.>>vv..
        |..>>v>>vv.
        |..>>>>>vv.
        |v......>vv
        |v>v....>>v
        |vvv.....>>
        |>vv......>
        |.>v.vv.v..""".stripMargin == board.steps(58).toString)

    var initial = parseBoard(scala.io.Source.fromFile("week4/input25.txt").getLines().map(_.trim))
    var c = 1
    while (true) {
      val n = initial.next
      if (n.toString == initial.toString) {
        println(c)
        return
      }
      initial = n
      c += 1
    }
  }


}
