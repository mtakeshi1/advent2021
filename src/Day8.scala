object Day8 {

  /*


  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg




   * 2 -> 5 digits
   * 3 -> 5 digits
   * 5 -> 5 digits

   * 0 -> 6 digits
   * 6 -> 6 digits
   * 9 -> 6 digits
   */

  def minus(left: List[Char], right: List[Char]): List[Char] = left.filter(c => !right.contains(c))


  def parseB(st: String) = {
    val inputSignals = st.split("[|]")(0).split(" ").map(_.trim).map(_.toList.distinct.sorted).toList

    val one = inputSignals.find(_.size == 2).get
    val four = inputSignals.find(_.size == 4).get
    val seven = inputSignals.find(_.size == 3).get
    val eight = inputSignals.find(_.size == 7).get
    val nine = inputSignals.filter(_.size == 6).find(digit => minus(digit, four).size == 2).get
    val zero = inputSignals.filter(_.size == 6).filterNot(_ == nine).find(minus(_, one).size == 4).get
    val six = inputSignals.filter(_.size == 6).filterNot(_ == nine).find(minus(_, one).size == 5).get
    val two = inputSignals.filter(_.size == 5).find(minus(_, nine).size == 1).get
    val three = inputSignals.filter(_.size == 5).filterNot(_ == two).find(minus(_, one).size == 3).get
    val five = inputSignals.filter(_.size == 5).filterNot(_ == two).find(minus(_, one).size == 4).get

    val outputSignals = st.split("[|]")(1).split(" ").map(_.trim).filter(_.size > 0).map(_.toList.distinct.sorted).toList
    val mapping = Map(zero -> 0, one -> 1, two -> 2, three -> 3, four -> 4, five -> 5, six -> 6, seven -> 7, eight -> 8, nine -> 9)
    def calc(digits: List[List[Char]], acc: Int = 0): Int = {
      digits match {
        case Nil => acc
        case head :: next => calc(next, acc * 10 + mapping(head))
      }
    }
    calc(outputSignals)
  }


  def parseLine(st: String): List[List[Char]] = {
    st.split("[|]")(1).split(" ").map(_.trim).map(_.toList.distinct).toList
  }

  def find(st: String): Int = {
    val parsed = parseLine(st)
    parsed.count(l => l.size == 2 || l.size == 4 || l.size == 3 || l.size == 7)
  }

  def main(args: Array[String]): Unit = {
//    println(parseB("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"))
//    println(scala.io.Source.fromFile("sample8.txt").getLines().map(_.trim).map(find).sum)
    println(scala.io.Source.fromFile("input8.txt").getLines().map(_.trim).map(parseB).sum)
  }


}
