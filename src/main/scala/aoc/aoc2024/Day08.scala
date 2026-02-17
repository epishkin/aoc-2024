package aoc.aoc2024

object Day08 {
  private val TEST_FILE = "day08-test.txt"
  private val DATA_FILE = "day08-data.txt"

  def main(args: Array[String]): Unit = {
    assertResult(14, countPart1(TEST_FILE))
    assertResult(34, countPart2(TEST_FILE))

    println(countPart1(DATA_FILE))
    println(countPart2(DATA_FILE))
  }

  private case class Antenna(i: Int, j: Int, power: Char)

  private def countPart1(inputFile: String) = {
    countAntinodes(inputFile, findAntinodes)
  }

  private def countPart2(inputFile: String) = {
    countAntinodes(inputFile, findAntinodesWithResonantHarmonics)
  }

  private type AntinodesFinder = (Antenna, Antenna, (Int, Int), (Int, Int)) => List[(Int, Int)]

  private def countAntinodes(inputFile: String, finder: AntinodesFinder) = {
    val input = readFile(inputFile).toList
    val allAntennas = input.zipWithIndex.flatMap { case (row, i) =>
      row.zipWithIndex.filter(_._1 != '.').map { case (power, j) =>
        Antenna(i, j, power)
      }
    }

    val height = input.length
    val width = input.head.length
    def inRange(i: Int, j: Int) = {
      i >= 0 && j >= 0 && i < height && j < width
    }

    val antiNodes = allAntennas
      .groupBy(_.power)
      .map { case (_, antennas) =>
        antennas.combinations(2).flatMap { case List(a1, a2) =>
          finder(a1, a2, (a2.i - a1.i, a2.j - a1.j), (height, width))
        }
      }
      .flatten
      .filter { case (i, j) => inRange(i, j) }
      .toSet

    antiNodes.size
  }

  private def findAntinodes(a1: Antenna, a2: Antenna, vec: (Int, Int), dimensions: (Int, Int)): List[(Int, Int)] = {
    List(
      (a1.i - vec._1, a1.j - vec._2),
      (a2.i + vec._1, a2.j + vec._2)
    )
  }

  private def findAntinodesWithResonantHarmonics(a1: Antenna, a2: Antenna, vec: (Int, Int), dimensions: (Int, Int)): List[(Int, Int)] = {
    val (height, width) = dimensions
    val maxK = Math.max(height / vec._1, width / vec._2) //we could do better but this is good enough
    (0 to maxK).flatMap { k =>
      List(
        (a1.i - k * vec._1, a1.j - k * vec._2),
        (a2.i + k * vec._1, a2.j + k * vec._2)
      )
    }.toList
  }
}
