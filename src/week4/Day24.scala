package week4

import week4.Day24.Literal

import java.beans.Expression
import java.util.Date
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Day24 {

  val maxCacheSize = 12

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  object SolutionSet {
    val all = SolutionSet(0.until(14).map(i => (i, 1.to(9).toSet)).toMap)
    val none = SolutionSet(Map())
  }

  case class SolutionSet(inputMap: Map[Int, Set[Int]]) {
    assert(inputMap.size <= 14)

    def and(other: SolutionSet): SolutionSet = {
      SolutionSet(inputMap.keys.filter(other.inputMap.contains).map(digit => {
        (digit, inputMap(digit).filter(other.inputMap(digit).contains))
      }).toMap)
    }

    def or(other: SolutionSet): SolutionSet = {
      SolutionSet((inputMap.keys ++ other.inputMap.keys).toSet.map(key => {
        (key, (inputMap.get(key), other.inputMap.get(key)) match {
          case (Some(set0), Some(set1)) => set0 ++ set1
          case (Some(set), _) => set
          case (_, Some(set)) => set
          case _ => sys.error("???")
        })
      }).toMap)
    }

    def complementary(): SolutionSet = {
      SolutionSet(0.until(14).map(i => {
        (i, 1.to(9).toSet -- this.inputMap.get(i).getOrElse(Set()))
      }).toMap)
    }

  }

  def allPossibleInputs(inputVariables: List[Int], m: Map[Int, Int] = Map()): LazyList[Map[Int, Int]] = {
    inputVariables match {
      case Nil => LazyList(m)
      case v :: r => {
        Range(9, 0, -1).map(vv => m.updated(v, vv)).flatMap(mm => allPossibleInputs(r, mm)).to(LazyList)
      }
    }
  }

  case class Domain(min: Double = Double.NegativeInfinity, max: Double = Double.PositiveInfinity) {
    def overlaps(other: Domain) = {
      (other.min >= this.min && this.max >= other.min) || (this.min >= other.min && other.max >= this.min)
    }

    def contains(n: Int) = this.min <= n && this.max >= n

  }

  trait Expression {
    //    private val evalCache = scala.collection.concurrent.TrieMap[Map[Int, Int], Int]()
    private val evalCache = scala.collection.mutable.Map[Map[Int, Int], Int]()
    private val isZeroCache = scala.collection.mutable.Map[Map[Int, Int], Boolean]()

    def eval(s: String): Int = {
      assert(s.size == 14)
      val input = s.toList.map(_ - '0').zipWithIndex.map(_.swap).toMap
      evalCached(input)
    }

    lazy val numNodes: Int = 1;

    lazy val solutionsToZero: SolutionSet = SolutionSet.all

    def cachedIsZero(in: Map[Int, Int]): Boolean = {
      if (cache2 != null && cache2.contains(in)) {
        cache2(in)
      } else if (usedInputVariablesForIsZero.size > maxCacheSize) isZero(in)
      else {
        val copy = in.filter(t => this.usedInputVariablesForIsZero.contains(t._1))
        isZeroCache.getOrElseUpdate(copy, isZero(copy))
      }
    }

    def isZero(in: Map[Int, Int]): Boolean = {
      canBeZero && evalCached(in) == 0
    }

    val canBeNegative = true

    val canBeZero: Boolean = true

    def evalCached(inputVariables: Map[Int, Int]): Int = {
      if (usedInputVariables.size > maxCacheSize) eval(inputVariables) else {
        val copy = inputVariables.filter(t => this.usedInputVariables.contains(t._1))
        evalCache.getOrElseUpdate(copy, eval(copy))
      }
    }

    def evalLater(inputVariables: Map[Int, Int]): Future[Int] = Future(eval(inputVariables))

    def eval(inputVariables: Map[Int, Int]): Int

    def cFold: Expression = this

    def usedInputVariables: Set[Int]

    lazy val usedInputVariablesForIsZero: Set[Int] = usedInputVariables

    var cache2: scala.collection.mutable.Map[Map[Int, Int], Boolean] = null

    lazy val domain: Domain = Domain()

  }

  trait BinaryExpression extends Expression {
    def left: Expression

    def right: Expression

    override lazy val numNodes: Int = 1 + left.numNodes + right.numNodes

    override lazy val usedInputVariables: Set[Int] = left.usedInputVariables ++ right.usedInputVariables

    def combine(l: Int, right: Int): Int

    override def evalLater(inputVariables: Map[Int, Int]): Future[Int] = {
      val lf: Future[Int] = left.evalLater(inputVariables)
      val rf: Future[Int] = right.evalLater(inputVariables)
      lf.flatMap(t => rf.map(combine(t, _)))
    }

    override def eval(inputVariables: Map[Int, Int]): Int = {
      combine(left.eval(inputVariables), right.eval(inputVariables))
    }

    override lazy val usedInputVariablesForIsZero: Set[Int] = left.usedInputVariablesForIsZero ++ right.usedInputVariablesForIsZero
  }


  case class Input(index: Int) extends Expression {

    override def isZero(in: Map[Int, Int]): Boolean = false

    override lazy val domain: Domain = Domain(1, 9)

    override lazy val solutionsToZero: SolutionSet = SolutionSet.none

    override val canBeZero: Boolean = false

    override val canBeNegative: Boolean = false

    override def eval(inputVariables: Map[Int, Int]): Int = inputVariables(index)

    override def evalCached(inputVariables: Map[Int, Int]): Int = inputVariables(index)

    override def usedInputVariables: Set[Int] = Set(index)

    override def toString: String = s"In_$index"
  }

  case class Add(left: Expression, right: Expression) extends BinaryExpression {
    override lazy val domain: Domain = Domain(left.domain.min + right.domain.min, left.domain.max + right.domain.max)

    override def combine(l: Int, r: Int): Int = l + r

    override lazy val solutionsToZero: SolutionSet = {
      if (!left.canBeNegative && !right.canBeNegative) {
        left.solutionsToZero.and(right.solutionsToZero)
      } else if (usedInputVariablesForIsZero.size < 7) {
        val possible: Seq[Map[Int, Int]] = allPossibleInputs(usedInputVariablesForIsZero.toList).filter(m => this.evalCached(m) == 0)
        val value: Map[Int, Seq[(Int, Int)]] = possible.flatMap(_.toList).groupBy(_._1)
        val map: Map[Int, Set[Int]] = value.view.mapValues(_.map(_._2).toSet).toMap
        SolutionSet(map)
      } else {
        //        sys.error(s"input size: ${usedInputVariablesForIsZero.size}")
        SolutionSet.all
      }

    }

    override def isZero(in: Map[Int, Int]): Boolean = {
      if (!left.canBeNegative && !right.canBeNegative) {
        //the must both be zero
        right.cachedIsZero(in) && left.cachedIsZero(in)
      } else if (!left.canBeZero && !right.canBeZero) {
        false;
      } else {
        super.isZero(in)
      }
    }

    override val canBeNegative: Boolean = left.canBeNegative || right.canBeNegative

    override val canBeZero: Boolean = {
      (left.canBeZero && right.canBeZero) || (left.canBeNegative || right.canBeNegative)
    }

    override def cFold: Expression = (left, right) match {
      case (Literal(l), Literal(r)) => Literal(l + r)
      case (Literal(0), r) => r
      case (l, Literal(0)) => l
      case (l, r) => this
    }

    override def toString: String = {
      (left, right) match {
        case (Literal(l), Literal(r)) => s"${l + r}"
        case (Literal(0), r) => s"${r}"
        case (l, Literal(0)) => s"${l}"
        case (l, r) => s" ( $left + $right ) "
      }
    }

  }

  case class Mul(left: Expression, right: Expression) extends BinaryExpression {
    override def combine(l: Int, r: Int): Int = l * r

    override def isZero(in: Map[Int, Int]): Boolean = {
      (right.canBeZero && right.cachedIsZero(in)) || (left.canBeZero && left.cachedIsZero(in))
    }

    override lazy val solutionsToZero: SolutionSet = {
      val l = if (left.canBeZero) left.solutionsToZero else SolutionSet.none
      val r = if (right.canBeZero) right.solutionsToZero else SolutionSet.none
      l.or(r)
    }

    override lazy val usedInputVariablesForIsZero: Set[Int] = {
      if (!right.canBeZero) left.usedInputVariablesForIsZero
      else if (!left.canBeZero) right.usedInputVariablesForIsZero
      else usedInputVariables
    }
    override val canBeNegative: Boolean = left.canBeNegative || right.canBeNegative

    override val canBeZero: Boolean = left.canBeZero || right.canBeZero

    //    override def eval(inputVariables: Map[Int, Int]): Int = {
    //      val l = left.evalCached(inputVariables)
    //      if (l == 0) 0
    //      else l * right.evalCached(inputVariables)
    //    }

    override def cFold: Expression = (left, right) match {
      case (Literal(l), Literal(r)) => Literal(l * r)
      case (Literal(0), _) => Literal(0)
      case (_, Literal(0)) => Literal(0)
      case (Literal(1), r) => r
      case (l, Literal(1)) => l
      case (l, r) => this
    }

    override def toString: String = (left, right) match {
      case (Literal(l), Literal(r)) => Literal(l * r).toString
      case (Literal(0), _) => Literal(0).toString
      case (_, Literal(0)) => Literal(0).toString
      case (Literal(1), r) => r.toString
      case (l, Literal(1)) => l.toString
      case (l, r) => s" ( $l * $r ) "
    }


  }

  case class Div(left: Expression, right: Expression) extends BinaryExpression {
    override def combine(l: Int, r: Int): Int = l / r

    override lazy val domain: Domain = Domain(0, Double.PositiveInfinity)

    override def isZero(in: Map[Int, Int]): Boolean = left.cachedIsZero(in) || left.evalCached(in) < right.evalCached(in)

    override val canBeZero: Boolean = true

    override val canBeNegative: Boolean = left.canBeNegative || right.canBeNegative

    //    override def eval(inputVariables: Map[Int, Int]): Int = {
    //      val l = left.evalCached(inputVariables)
    //      if (l == 0) 0
    //      else l / right.evalCached(inputVariables)
    //    }

    override def cFold: Expression = (left, right) match {
      case (l, Literal(1)) => l
      case (Literal(l), Literal(r)) => Literal(l / r)
      case (l, r) => if (l == r) Literal(1) else this
    }

    override def toString: String = (left, right) match {
      case (l, Literal(1)) => l.toString
      case (Literal(l), Literal(r)) => Literal(l / r).toString
      case (l, r) => if (l == r) Literal(1).toString else s" ( $l / $r ) "
    }

  }

  case class Mod(left: Expression, right: Expression) extends BinaryExpression {
    override def combine(l: Int, r: Int): Int = l % r

    override def isZero(in: Map[Int, Int]): Boolean = left.cachedIsZero(in) || right.evalCached(in) == 1 || (left.evalCached(in) % right.evalCached(in)) == 0

    override lazy val domain: Domain = Domain(0, right.domain.max - 1)

    override val canBeZero: Boolean = true

    override val canBeNegative: Boolean = false

    //    override def eval(inputVariables: Map[Int, Int]): Int = {
    //      val l = left.evalCached(inputVariables)
    //      if (l == 0) 0
    //      else l % right.evalCached(inputVariables)
    //    }

    override def cFold: Expression = (left, right) match {
      case (Literal(0), _) => Literal(0)
      case (l, Literal(1)) => Literal(0)
      case (Literal(l), Literal(r)) => Literal(l % r)
      case (l, r) => if (l == r) Literal(0) else this
    }

    override def toString: String = (left, right) match {
      case (Literal(0), _) => Literal(0).toString
      case (l, Literal(1)) => Literal(0).toString
      case (Literal(l), Literal(r)) => Literal(l % r).toString
      case (l, r) => if (l eq r) Literal(0).toString else s" ( $l % $r ) "
    }

  }

  case class Eql(left: Expression, right: Expression) extends BinaryExpression {

    override val canBeNegative: Boolean = false

    override lazy val domain: Domain = Domain(0, 1)

    override def isZero(in: Map[Int, Int]): Boolean = {
      (left, right) match {
        case (l, Literal(0)) => !l.cachedIsZero(in)
        case (Literal(0), r) => !r.cachedIsZero(in)
        case _ => left != right && left.evalCached(in) != right.evalCached(in)
      }

    }

    override def combine(l: Int, r: Int): Int = if (l == r) 1 else 0
    //    override def eval(inputVariables: Map[Int, Int]): Int = if (left.evalCached(inputVariables) == right.evalCached(inputVariables)) 1 else 0

    override def cFold: Expression = (left, right) match {
      case (Literal(l), Literal(r)) => if (l == r) Literal(1) else Literal(0)
      case (eq: Eql, Literal(1)) => eq
      case (l, r) => if (l eq r) Literal(1) else if (l.domain.overlaps(r.domain)) this else Literal(0)
    }

    override def toString: String = (left, right) match {
      case (Literal(l), Literal(r)) => if (l == r) Literal(1).toString else Literal(0).toString
      case (l, r) => if (l eq r) Literal(1).toString else s" ( $l == $r ) "
    }

  }

  object NoOp extends Expression {


    override def evalCached(inputVariables: Map[Int, Int]): Int = ???

    override def evalLater(inputVariables: Map[Int, Int]): Future[Int] = ???

    override def eval(inputVariables: Map[Int, Int]): Int = ???

    override def usedInputVariables: Set[Int] = Set()
  }

  case class Literal(lit: Int) extends Expression {

    override lazy val domain: Domain = Domain(lit, lit)

    override def isZero(in: Map[Int, Int]): Boolean = lit == 0

    override def eval(inputVariables: Map[Int, Int]): Int = lit

    override val canBeZero: Boolean = lit == 0
    override val canBeNegative: Boolean = lit < 0

    override def evalLater(inputVariables: Map[Int, Int]): Future[Int] = Future.successful(lit)

    override def evalCached(inputVariables: Map[Int, Int]): Int = lit

    override def usedInputVariables: Set[Int] = Set()

    override def toString: String = s"$lit"
  }

  case class Env(variables: Map[String, Expression] = Map().withDefaultValue(Literal(0)), nextInput: Int = 0, lastExpression: Expression = NoOp) {
    def addVariable(name: String, expression: Expression) = Env(variables.updated(name, expression), nextInput)

    def varOrLiteral(n: String): Expression = {
      if (n.matches("-?[0-9]+")) Literal(n.toInt)
      else variables(n)
    }

    def addExpression(name: String, expression: Expression) = {
      val folded = expression.cFold;
      copy(variables = variables.updated(name, folded), lastExpression = folded)
    }

  }

  def createEnv(meat: List[String]): Env = {
    val env = Env(Map("z" -> Input(1), "w" -> Input(0), "x" -> Literal(0), "y" -> Literal(0)))
    val newEnv = meat.foldLeft(env)((env, line) => parseLine(line, env))
    //    newEnv.lastExpression.eval(Map(0 -> nextInput))
    newEnv
  }

  def createEnv(z: Int, digit: Int, meat: List[String]): Env = {
    val env = Env(Map("z" -> Literal(z), "w" -> Literal(digit), "x" -> Literal(0), "y" -> Literal(0)))
    val newEnv = meat.foldLeft(env)((env, line) => parseLine(line, env))
    //    newEnv.lastExpression.eval(Map(0 -> nextInput))
    newEnv
  }

  case class Variables(x: Int, z: Int)

  def findHighest(previousRound: scala.collection.Map[Int, Long], lines: Iterator[String]): Map[Int, Long] = {
    val meat = lines.dropWhile(_.startsWith("inp")).takeWhile(!_.startsWith("inp")).toList
    val env = createEnv(meat)
    val allResults: Iterable[(Int, Long)] = (for {
      z <- previousRound.keys
      digit <- 1.to(9)
      //      env = createEnv(z, digit, meat)
      res = env.lastExpression.eval(Map(0 -> digit, 1 -> z))
      if (meat.contains("div z 1") || res < z)
    } yield (res -> (previousRound.getOrElse(z, 0L) * 10 + digit)))
    allResults.groupBy(_._1).map(t => (t._1, t._2.map(_._2))).map(t => (t._1, t._2.max)).toMap
  }

  def findLowestMutable(previousRound: scala.collection.mutable.Map[Int, Long], lines: Iterator[String]): scala.collection.mutable.Map[Int, Long] = {
    val meat = lines.dropWhile(_.startsWith("inp")).takeWhile(!_.startsWith("inp")).toList
    val env = createEnv(meat)
    var map = scala.collection.mutable.Map[Int, Long]()
    for {
      z <- previousRound.keys
      digit <- 1.to(9)
      res = env.lastExpression.eval(Map(0 -> digit, 1 -> z))
    } {
      val newSig = previousRound(z) * 10 + digit
      map.put(res, Math.min(map.getOrElse(res, Long.MaxValue), newSig))
    }
    map
  }

  def findHighestMutable(previousRound: scala.collection.mutable.Map[Int, Long], lines: Iterator[String]): scala.collection.mutable.Map[Int, Long] = {
    val meat = lines.dropWhile(_.startsWith("inp")).takeWhile(!_.startsWith("inp")).toList
    val env = createEnv(meat)
    var map = scala.collection.mutable.Map[Int, Long]()
    for {
      z <- previousRound.keys
      digit <- 1.to(9)
      res = env.lastExpression.eval(Map(0 -> digit, 1 -> z))
    } {
      val newSig = previousRound(z) * 10 + digit
      map.put(res, Math.max(map.getOrElse(res, 0L), newSig))
    }
    map
  }

  def executeFile(file: String) = {
    val lines = scala.io.Source.fromFile(file).getLines().map(_.trim)
    var lastRound: scala.collection.Map[Int, Long] = scala.collection.mutable.Map(0 -> 0L)
    var digits = 0
    while (lines.hasNext) {
      lastRound = findHighest(lastRound, lines)
      digits = digits + 1
      println(s"processed $digits - output has: ${lastRound.size}")
    }
    println(lastRound(0))
  }

  def parseLine(line: String, env: Env = Env()): Env = {
    line.split(" ").toList match {
      case "inp" :: variable :: Nil => Env(env.variables.updated(variable, Input(env.nextInput)), env.nextInput + 1)
      case "add" :: varleft :: varRight :: Nil => env.addExpression(varleft, Add(env.variables(varleft), env.varOrLiteral(varRight)))
      case "mul" :: varleft :: varRight :: Nil => env.addExpression(varleft, Mul(env.variables(varleft), env.varOrLiteral(varRight)))
      case "div" :: varleft :: varRight :: Nil => env.addExpression(varleft, Div(env.variables(varleft), env.varOrLiteral(varRight)))
      case "mod" :: varleft :: varRight :: Nil => env.addExpression(varleft, Mod(env.variables(varleft), env.varOrLiteral(varRight)))
      case "eql" :: varleft :: varRight :: Nil => env.addExpression(varleft, Eql(env.variables(varleft), env.varOrLiteral(varRight)))
      case _ => sys.error(s"could not parse ${line}")
    }
  }

  def parseFile(file: String): Expression = {
    scala.io.Source.fromFile(file).getLines().map(_.trim).foldLeft(Env())((env, line) => parseLine(line, env)).lastExpression
  }

  def toMap(l: Long) = {
    val s = l.toString
    val ss = "0" * (14 - s.size) + s
    0.until(14).map(t => t -> (ss(t) - '0')).toMap
  }

  def main(args: Array[String]): Unit = {
    executeFile("week4/input24.txt")
    //    val expression = parseFile("week4/input24.txt")
    //    //    println(expression)
    //    val used = expression.usedInputVariables;
    //    println(expression.numNodes)
    //    //    val unused = 0.until(14).filterNot(used.contains)
    //    //    println(unused)
    //
    //    var input = 0.until(14).map(_ => '9').mkString("").toLong
    //    //    val input = toMap("99999999999677".toLong)
    //    //    println(expression.eval(input))
    //    var time = System.currentTimeMillis();
    //
    //    def trySolve(in: Map[Int, Int]): Future[Int] = {
    //      if (in.values.count(_ == 0) == 0) {
    //        expression.evalLater(in)
    //      } else {
    //        Future.successful(-1)
    //      }
    //    }
    //
    //    var lastCount = input;
    //    while (input >= 11111111111111L) {
    //      val m: Map[Int, Int] = toMap(input)
    //      assert(m.size == 14)
    //      if (m.values.count(_ == 0) == 0) {
    //        if (expression.cachedIsZero(m)) {
    //          println(input)
    //          return
    //        }
    //        if (System.currentTimeMillis() > time + 10000) {
    //          println(s"${new Date()} current input: $input - gone through: ${lastCount - input}")
    //          lastCount = input
    //          time = System.currentTimeMillis();
    //        }
    //      }
    //      input = input - 1;
    //    }
    //
    //    println(expression.eval(0.until(14).map(_ -> 9).toMap))
  }


}
