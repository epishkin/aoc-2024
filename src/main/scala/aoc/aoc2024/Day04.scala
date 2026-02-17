package aoc.aoc2024

object Day04 {
  private val TEST_FILE = "day04-test.txt"
  private val DATA_FILE = "day04-data.txt"

  def main(args: Array[String]): Unit = {
    assertResult(18, countWordsPart1(TEST_FILE))
    assertResult(9, countWordsPart2(TEST_FILE))

    println(countWordsPart1(DATA_FILE))
    println(countWordsPart2(DATA_FILE))
  }

  private def countWordsPart1(inputFile: String) = {
    val XMAS = "XMAS"
    val SAMX = XMAS.reverse
    val WORD_LENGTH = XMAS.length

    // (di, dj) are the direction vectors
    val vectors = Array((1, 0), (0, 1), (1, 1), (-1, 1))

    val rows = readFile(inputFile).toArray
    val height = rows.length

    def extractWord(i: Int, j: Int, di: Int, dj: Int): String = {
      val word = new StringBuilder
      for (k <- 0 until WORD_LENGTH) {
        val c = i + k*di
        val r = j + k*dj
        if (r < height) {
          val row = rows(r)
          if (c >= 0 && c < row.length) {
            word.append(row(c))
          }
        }
      }
      word.toString
    }

    var count = 0
    rows.zipWithIndex.foreach { case (row, i) =>
      for (j <- 0 until row.length) {
        vectors.foreach { case (di, dj) =>
          val extracted = extractWord(i, j, di, dj)
          if (extracted == XMAS || extracted == SAMX) {
            count += 1
          }
        }
      }
    }
    count
  }

  private case class Cell(r: Int, c: Int, value: Char)

  private def countWordsPart2(inputFile: String) = {
    //matching patterns for X-MAS
    val patterns = List(
      List(
        Cell(0, 0, 'M'), Cell(0, 2, 'S'),
        Cell(1, 1, 'A'),
        Cell(2, 0, 'M'), Cell(2, 2, 'S')
      ),
      List(
        Cell(0, 0, 'S'), Cell(0, 2, 'M'),
        Cell(1, 1, 'A'),
        Cell(2, 0, 'S'), Cell(2, 2, 'M')
      ),
      List(
        Cell(0, 0, 'M'), Cell(0, 2, 'M'),
        Cell(1, 1, 'A'),
        Cell(2, 0, 'S'), Cell(2, 2, 'S')
      ),
      List(
        Cell(0, 0, 'S'), Cell(0, 2, 'S'),
        Cell(1, 1, 'A'),
        Cell(2, 0, 'M'), Cell(2, 2, 'M')
      )
    )

    val rows = readFile(inputFile).toArray
    val height = rows.length

    def isMatch(i: Int, j: Int, pattern: List[Cell]): Boolean = {
      pattern.forall { cell =>
        val r = i + cell.r
        val c = j + cell.c
        r >= 0 && r < height && c >= 0 && c < rows(r).length && rows(r)(c) == cell.value
      }
    }

    var count = 0
    rows.zipWithIndex.foreach { case (row, i) =>
      for (j <- 0 until row.length) {
        patterns.foreach { pattern =>
          if (isMatch(i, j, pattern)) {
            count += 1
          }
        }
      }
    }

    count
  }
}