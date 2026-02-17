package aoc.aoc2024

object Day07 {
  private val TEST_FILE = "day07-test.txt"
  private val DATA_FILE = "day07-data.txt"

  def main(args: Array[String]): Unit = {
    assertResult(3749, countPart1(TEST_FILE))
    assertResult(11387, countPart2(TEST_FILE))

    println(countPart1(DATA_FILE))
    println(countPart2(DATA_FILE))
  }

  private type Operation = (Long, Long) => Long
  private val ADD: Operation = _ + _
  private val MUL: Operation = _ * _
  private val CONCAT: Operation = (a:Long, b:Long) => (a.toString + b.toString).toLong
  private val OPERATIONS = List[Operation](ADD, MUL)
  private val OPERATIONS2 = OPERATIONS.appended(CONCAT)

  private def countPart1(inputFile: String) = {
    calculateSumOfTrueEquations(inputFile, OPERATIONS)
  }

  private def countPart2(inputFile: String) = {
    calculateSumOfTrueEquations(inputFile, OPERATIONS2)
  }

  private def calculateSumOfTrueEquations(inputFile: String, operations: List[Operation]) = {
    readFile(inputFile).toArray
      .map {
        _.split(":") match {
          case Array(total, values) =>
            (total.toLong, values.split(" ").filter(_.nonEmpty).map(_.trim.toLong))
        }
      }
      .filter { case (total, values) =>
        canBeTrueEquation(total, 0, values.toList, operations)
      }
      .map(_._1)
      .sum
  }

  private def canBeTrueEquation(expectedTotal: Long, total: Long, values: List[Long], operations: List[Operation]): Boolean = {
    if (total > expectedTotal) {
      false
    } else {
      values match {
        case Nil => total == expectedTotal

        case head :: tail =>
          operations.exists { op =>
            canBeTrueEquation(expectedTotal, op(total, head), tail, operations)
          }
      }
    }
  }
}
