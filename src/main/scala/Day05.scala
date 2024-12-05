package aoc2024

object Day05 {
  private val TEST_FILE = "day05-test.txt"
  private val DATA_FILE = "day05-data.txt"

  def main(args: Array[String]): Unit = {
    val (goodSum, correctedSum) = calculateSums(TEST_FILE)
    assertResult(143, goodSum)
    assertResult(123, correctedSum)

    println(calculateSums(DATA_FILE))
  }

  private def calculateSums(inputFile: String) = {
    val rows = readFile(inputFile).toList

    val (rulesTxt, pagesTxt) = rows.span(_.trim.nonEmpty)

    val ruleRegex = """(\d+)\|(\d+)""".r
    val rules = rulesTxt
      .filter(_.nonEmpty)
      .map { case ruleRegex(before, after) =>
        (before.toInt, after.toInt)
      }

    val updatedPages = pagesTxt
      .filter(_.nonEmpty)
      .map(_.split(",").map(_.toInt).toList)

    def isCorrectOrder(a: Int, b: Int) = {
      rules.indexOf((a, b)) != -1
    }

    val (goodUpdatedPages, badPages) = updatedPages.partition(
      _.sliding(2).forall { case List(a, b) => isCorrectOrder(a, b) }
    )

    val correctedPages = badPages.map(
      _.sortWith { case (a, b) => isCorrectOrder(a, b) }
    )

    def calcSumOfMiddlePages(pages: List[List[Int]]) = {
      pages.map(ps => ps(ps.length / 2)).sum
    }

    (calcSumOfMiddlePages(goodUpdatedPages), calcSumOfMiddlePages(correctedPages))
  }
}
