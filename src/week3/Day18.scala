package week3

object Day18 {

  case class ExplosionResult(node: Node, left: Int, right: Int)

  trait Node {
    def canExplode(depth: Int = 0): Boolean

    def canSplit: Boolean

    def split: Node

    def explode(depth: Int = 0): ExplosionResult

    def addLeft(v: Int): Node

    def addRight(v: Int): Node

    def plus(n: Node) = {
      var root: Node = INode(this, n)
      while (root.canExplode() || root.canSplit) {
        if (root.canExplode()) root = root.explode().node
        else if (root.canSplit) root = root.split
      }
      root
    }

    def magnitude: Long

  }

  case class INode(left: Node, right: Node) extends Node {
    override def canSplit: Boolean = left.canSplit || right.canSplit

    override def canExplode(depth: Int = 0): Boolean = depth >= 4 || left.canExplode(depth + 1) || right.canExplode(depth + 1)

    override def split: Node = if (left.canSplit) INode(left.split, right) else if (right.canSplit) INode(left, right.split) else this

    override def addLeft(v: Int): Node = INode(left.addLeft(v), right)

    override def addRight(v: Int): Node = INode(left, right.addRight(v))

    def explodeHere(): ExplosionResult = {
      (left, right) match {
        case (Leaf(vl), Leaf(vr)) => ExplosionResult(Leaf(0), vl, vr)
        case (n: INode, _) => {
          val result = n.explodeHere()
          val newRight = right.addLeft(result.right)
          ExplosionResult(INode(result.node, newRight), result.left, 0)
        }
        case (Leaf(v), n: INode) => {
          val result = n.explodeHere()
          val newLeft = Leaf(v + result.left)
          ExplosionResult(INode(newLeft, result.node), 0, result.right)
        }
        case _ => ExplosionResult(this, 0, 0)
      }

    }

    override def toString: String = s"[$left,$right]"

    override def explode(depth: Int = 0): ExplosionResult = {
      if (depth > 3) {
        explodeHere()
      } else {
        (left, right) match {
          case (n: INode, _) if (n.canExplode(depth + 1)) => {
            val result = n.explode(depth + 1)
            val newRight = right.addLeft(result.right)
            ExplosionResult(INode(result.node, newRight), result.left, 0)
          }
          case (_, n: INode) if (n.canExplode(depth + 1)) => {
            val result = n.explode(depth + 1)
            val newLeft = left.addRight(result.left)
            ExplosionResult(INode(newLeft, result.node), 0, result.right)
          }
          case _ => ExplosionResult(this, 0, 0)
        }
      }
    }

    override def magnitude: Long = 3 * left.magnitude + 2 * right.magnitude
  }

  case class Leaf(value: Int) extends Node {
    override def canExplode(depth: Int = 0): Boolean = false

    override def canSplit: Boolean = value >= 10

    override def split: Node = INode(Leaf(value / 2), Leaf(Math.ceil(value / 2.0).toInt))

    override def explode(depth: Int = 0): ExplosionResult = sys.error("leaf node cannot explode")

    override def addLeft(v: Int): Node = Leaf(value + v)

    override def addRight(v: Int): Node = Leaf(value + v)

    override def magnitude: Long = value

    override def toString: String = value.toString
  }


  def parseNumber(list: List[Char]): (Node, List[Char]) = {
    val num = list.takeWhile(c => c >= '0' && c <= '9').mkString("")
    val rest = list.drop(num.size)
    (Leaf(num.toInt), rest)
  }

  def parseNode(input: String): Node = {
    val r = parseNode(input.toList)
    assert(r._2.isEmpty, s"parse should be total, but ${r._2.mkString("")} was left")
    r._1
  }

  def parseNode(list: List[Char]): (Node, List[Char]) = {
    assert(list.head == '[', s"Node should start with [ but was ${list.head}")
    val (leftHandSide: Node, rest: scala.List[Char]) = parsePiece(list.tail)
    assert(!rest.isEmpty, "rest is empty =(")
    assert(rest.head == ',', s"rest should start with , to parse right hand side but was ${rest.head}")
    val (rightHandSide: Node, rest2: List[Char]) = parsePiece(rest.tail)
    assert(!rest2.isEmpty, "rest is empty =(")
    assert(rest2.head == ']', s"list should end with ] but was ${rest2.head}")
    (INode(leftHandSide, rightHandSide), rest2.tail)
  }

  private def parsePiece(list: List[Char]): (Node, List[Char]) = {
    list match {
      case '[' :: rest => parseNode(list)
      case c :: rest if c >= '0' && c <= '9' => parseNumber(list)
      case _ => sys.error("cant start parsing left hand side from: " + list.mkString(""))
    }
  }

  def sumAll(input: List[Node]): Node = {
    input.tail.foldLeft(input.head)((l, r) => l.plus(r))
  }

  def allCombinations(input: List[Node]): Seq[(Node, Node)] = {
    val array = input.toArray
    for {
      i <- 0.until(array.length)
      j <- 0.until(array.length)
      if i != j
    } yield (array(i), array(j))
  }

  def main(args: Array[String]): Unit = {
    assert(parseNode("[1,2]") == INode(Leaf(1), Leaf(2)))
    assert(parseNode("[[[[9,1],2],3],4]").canExplode() == false)
    assert(parseNode("[[[[[9,8],1],2],3],4]").canExplode() == true)
    assert(parseNode("[[[[[9,8],1],2],3],4]").explode().node == parseNode("[[[[0,9],2],3],4]"))
    assert(parseNode("[7,[6,[5,[4,[3,2]]]]]").explode().node == parseNode("[7,[6,[5,[7,0]]]]"))
    assert(parseNode("[[6,[5,[4,[3,2]]]],1]").explode().node == parseNode("[[6,[5,[7,0]]],3]"))
    assert(parseNode("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]").explode().node == parseNode("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"))
    assert(parseNode("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]").explode().node == parseNode("[[3,[2,[8,0]]],[9,[5,[7,0]]]]"))

    assert(parseNode("[[[[4,3],4],4],[7,[[8,4],9]]]").plus(parseNode("[1,1]")) == parseNode("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"))

    val input = scala.io.Source.fromFile("week3/sample18.txt").getLines().map(_.trim).map(parseNode).toList
    val node = sumAll(input)
    assert(node == parseNode("[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]"))
    assert(node.magnitude == 4140L)

    val max = allCombinations(input).map(t => t._1.plus(t._2)).map(_.magnitude).max
    assert(max == 3993)
    println(max)
    val nodeList = scala.io.Source.fromFile("week3/input18.txt").getLines().map(_.trim).filter(_.size > 0).map(parseNode).toList
    val value: Seq[(Node, Node)] = allCombinations(nodeList)
    println(value.map(t => t._1.plus(t._2)).map(_.magnitude).max)

  }

}
