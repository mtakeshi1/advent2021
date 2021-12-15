import scala.annotation.tailrec

object Day12 {

  var nodes = scala.collection.mutable.Map[String, List[String]]()

  def add(from: String, to: String) = {
    nodes.update(from, to :: nodes.getOrElse(from, List()))
    nodes.update(to, from :: nodes.getOrElse(to, List()))
  }

  def connections(node: String) = nodes.get(node).map(_.toSet).getOrElse(Set())

  def parse(file: String) = {
    scala.io.Source.fromFile(file).getLines().map(_.trim).foreach(line => {
      val parts = line.split("-")
      add(parts(0), parts(1))
    })
  }

  case class PartialPath(smallCaves: Map[String, Int]) {
    def canVisit(node: String): Boolean = {
      if(node == "start") false;
      else node.forall(_.isUpper) || !smallCaves.contains(node) || smallCaves.values.forall(_ < 2)
    }

    def visit(node: String) = {
      if(node.forall(_.isUpper)) this
      else PartialPath(smallCaves.updated(node, smallCaves.getOrElse(node, 0)+ 1))
    }
  }

  def findPathFrom(starting: String = "start", visited: Set[String] = Set(), soFar: List[String] = List()):List[List[String]] = {
    if(starting == "end") List(starting :: soFar)
    else {
      val filtered = if(starting.forall(_.isLower)) visited + starting else visited
      connections(starting).filterNot(filtered.contains).flatMap(node => {
        findPathFrom(node, filtered, starting :: soFar)
      }).filterNot(_.isEmpty).toList
    }
  }

  def findPathB(starting: String = "start", visited: PartialPath = PartialPath(Map()), soFar: List[String] = List()):List[List[String]] = {
    if(starting == "end") List(starting :: soFar)
    else {
      val filtered = visited.visit(starting)
      val toVisit = connections(starting).filter(filtered.canVisit)
      toVisit.flatMap(node => {
        findPathB(node, filtered, starting :: soFar)
      }).filterNot(_.isEmpty).toList
    }
  }


  def main(args: Array[String]): Unit = {
    parse("input12.txt")
    val path = findPathB()
    println(path.mkString("\n"))
    println(path.size)
  }

}
