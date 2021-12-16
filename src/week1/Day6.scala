package week1

object Day6 {

  def parse(line: String) = {
    val input = new Array[Long](10)
    line.split(",").map(_.toInt).foreach(i => input(i) += 1)
    input
  }

  def step(input: Array[Long]): Array[Long] = {
    val output = new Array[Long](input.length)
    output(6) = input(0)
    output(8) = input(0)
    0.until(9).foreach(i => {
      output(i) += input(i + 1)
    })
    output
  }

  def stepR(input: Array[Long], numSteps: Int): Array[Long] = {
    if (numSteps == 0) input
    else stepR(step(input), numSteps - 1)
  }


}
