package aoc2024

object Day03 {
  private val TEST_FILE = "day03-test.txt"
  private val TEST_FILE2 = "day03-test2.txt"
  private val DATA_FILE = "day03-data.txt"

  def main(args: Array[String]): Unit = {
    assertResult(161, compute(TEST_FILE))
    assertResult(48, computePart2(TEST_FILE2))

    println(compute(DATA_FILE))
    println(computePart2(DATA_FILE))
  }

  private def compute(inputFile: String) = {
      val pattern = "mul\\((\\d+),(\\d+)\\)".r
      readFile(inputFile)
        .flatMap(line =>
          pattern.findAllIn(line).toList
        )
        .map { txt =>
          val pattern(a, b) = txt
          a.toInt * b.toInt
        }
        .sum
  }

  private def computePart2(inputFile: String) = {
    val patternCommands = "do\\(\\)|don't\\(\\)|mul\\(\\d+,\\d+\\)".r
    val patternDo = "do\\(\\)".r
    val patternDont = "don't\\(\\)".r
    val patternMul = "mul\\((\\d+),(\\d+)\\)".r

    var mulEnabled = true
    readFile(inputFile)
      .flatMap(line =>
        patternCommands.findAllIn(line).toList
      )
      .map {
        case patternDo() =>
          mulEnabled = true
          0

        case patternDont() =>
          mulEnabled = false
          0

        case patternMul(a, b) if mulEnabled =>
          a.toInt * b.toInt

        case _ => 0
      }
      .sum
  }
}