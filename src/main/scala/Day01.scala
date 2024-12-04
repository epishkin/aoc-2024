package aoc2024

object Day01 {
  private val TEST_FILE = "day01-test.txt"
  private val DATA_FILE = "day01-data.txt"

  def main(args: Array[String]): Unit = {
    assertResult(11, computeTotalDistance(TEST_FILE))
    assertResult(31, computeSimilarityScore(TEST_FILE))

    println(computeTotalDistance(DATA_FILE))
    println(computeSimilarityScore(DATA_FILE))
  }

  private def computeTotalDistance(inputFile: String) = {
    val (left: List[Int], right: List[Int]) = parseInput(inputFile)

    val result = left.sorted
      .zip(right.sorted)
      .map { case (a, b) => (a - b).abs }
      .sum

    result
  }

  private def computeSimilarityScore(inputFile: String) = {
    val (left: List[Int], right: List[Int]) = parseInput(inputFile)

    val result = left
      .map { a =>
        right.count(_ == a) * a
      }
      .sum

    result
  }

  private def parseInput(inputFile: String) = {
    val rows = readFile(inputFile)
      .map(_.split("\\s+").filter(_.nonEmpty) match {
        case Array(a, b) => (a.toInt, b.toInt)
      })
      .toList

    (rows.map(_._1), rows.map(_._2))
  }
}