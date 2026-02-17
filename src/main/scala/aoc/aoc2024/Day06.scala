package aoc.aoc2024

import scala.collection.mutable

object Day06 {
  private val TEST_FILE = "day06-test.txt"
  private val DATA_FILE = "day06-data.txt"

  def main(args: Array[String]): Unit = {
    assertResult(41, countPart1(TEST_FILE))
    assertResult(6, countPart2(TEST_FILE))

    println(countPart1(DATA_FILE))
    println(countPart2(DATA_FILE))
  }

  private val GUARD = "^"
  private val OBSTACLE = '#'

  private def countPart1(inputFile: String) = {
    val rows = readFile(inputFile).toArray

    guardsPathLength(rows)._1
  }

  private def countPart2(inputFile: String) = {
    val rows = readFile(inputFile).toArray
    var count = 0
    findGuardPosition(rows).foreach { guardPos =>
      val aboveGuard = guardPos.copy(_1 = guardPos._1 - 1)
      rows.zipWithIndex.foreach { case (row, i) =>
        for (j <- 0 until row.length) {
          if (((i, j) != guardPos) && ((i, j) != aboveGuard) && (row.charAt(j) != OBSTACLE)) {
            if (guardsPathLength(rows, extraObstacle = Some((i, j)))._2 == GUARD_CYCLED) {
              count += 1
            }
          }
        }
      }
    }
    count
  }

  private val GUARD_WALKING = 0
  private val GUARD_EXITED = 1
  private val GUARD_CYCLED = 2

  private val DIRECTIONS = Array((-1, 0), (0, 1), (1, 0), (0, -1))

  //to avoid cloning the whole lab to add an extra obstacle we pass it as a parameter
  private def guardsPathLength(rows: Array[String], extraObstacle: Option[(Int, Int)] = None) = {
    val path = mutable.Set.empty[(Int, Int)]
    var guardState = GUARD_WALKING
    findGuardPosition(rows).foreach { start =>
      var currentPos = start
      var dirIdx = 0
      val cycles = mutable.Set.empty[(Int, Int, Int)] // (i, j, dirIdx)
      while (guardState == GUARD_WALKING) {
        path.add(currentPos)

        val nextI = currentPos._1 + DIRECTIONS(dirIdx)._1
        val nextJ = currentPos._2 + DIRECTIONS(dirIdx)._2
        if (nextI >= 0 && nextI < rows.length && nextJ >= 0 && nextJ < rows(nextI).length) {
          val cell = if (extraObstacle.contains((nextI, nextJ))) OBSTACLE else rows(nextI).charAt(nextJ)
          cell match {
            case OBSTACLE =>
              dirIdx = (dirIdx + 1) % DIRECTIONS.length
              if (cycles.contains((currentPos._1, currentPos._2, dirIdx))) {
                guardState = GUARD_CYCLED
              } else {
                cycles.add((currentPos._1, currentPos._2, dirIdx))
              }

            case _ =>
              currentPos = (nextI, nextJ)
          }
        } else {
          guardState = GUARD_EXITED
        }
      }
    }

    (path.size, guardState)
  }

  private def findGuardPosition(rows: Array[String]): Option[(Int, Int)] = {
    var guardPos: Option[(Int, Int)] = None
    rows.zipWithIndex.foreach { case (row, i) =>
      val idx = row.indexOf(GUARD)
      if (idx != -1) {
        guardPos = Some((i, idx))
      }
    }
    guardPos
  }
}
