import scala.io.Source

package object aoc2024 {
  def assertResult(expected: Int, actual: Int): Unit = {
    assert(expected == actual, s"Expected $expected but got $actual")
  }


  def readFile(inputFile: String): Iterator[String] = {
    Source.fromResource("aoc2024/" + inputFile)
      .getLines
  }
}
