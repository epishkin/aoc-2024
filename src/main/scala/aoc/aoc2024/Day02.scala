package aoc.aoc2024

object Day02 {
  private val TEST_FILE = "day02-test.txt"
  private val DATA_FILE = "day02-data.txt"

  def main(args: Array[String]): Unit = {
    assertResult(2, countGoodReports(TEST_FILE))
    assertResult(4, countGoodReportsWithDampener(TEST_FILE))

    println(countGoodReports(DATA_FILE))
    println(countGoodReportsWithDampener(DATA_FILE))
  }

  private def countGoodReports(inputFile: String) = {
    parseReports(inputFile).count(isReportSafe)
  }

  private def countGoodReportsWithDampener(inputFile: String) = {
    parseReports(inputFile).count { report =>
      isReportSafe(report) || isReportSafeIfOneLevelRemoved(report)
    }
  }

  private def isReportSafe(row: List[Int]) = {
    val diffs = row.sliding(2).map {
      case List(a, b) => b - a
    }.toList

    val allIncreasing = diffs.forall(diff => diff >= 1 && diff <= 3)
    val allDecreasing = diffs.forall(diff => diff <= -1 && diff >= -3)
    allIncreasing || allDecreasing
  }

  private def isReportSafeIfOneLevelRemoved(report: List[Int]) = {
    report.zipWithIndex.exists(pair => {
      val (_, idx) = pair
      isReportSafe(report.patch(idx, Nil, 1))
    })
  }

  private def parseReports(inputFile: String) = {
    readFile(inputFile)
      .map(_.split("\\s+").filter(_.nonEmpty).map(_.toInt).toList)
      .toList
  }
}
