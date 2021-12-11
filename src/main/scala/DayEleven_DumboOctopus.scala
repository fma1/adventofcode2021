import DayEleven_DumboOctopus.PartOne.{getFlashes, processCoordinate, setFlashesToZero}
import DayEleven_DumboOctopus.PartTwo.getFirstStepWithAllFlashing

import scala.collection.mutable

object DayEleven_DumboOctopus {
  val LENGTH = 10

  sealed trait ProcessCoordinateAction
  case object IncreaseEnergy extends ProcessCoordinateAction
  case object Flash extends ProcessCoordinateAction

  def parseData(filename: String): Array[Array[Int]] = {
    val bufferedSource = io.Source.fromInputStream(ClassLoader.getSystemResourceAsStream(filename))
    val lines = bufferedSource.getLines().toArray
      .map(_.trim.split("")
        .map(_.toInt))
    bufferedSource.close()
    lines
  }

  // For debugging purposes
  def printGrid(grid: Array[Array[Int]]): Unit = {
    println("\n\n")
    grid.foreach(row => println(row.mkString("")))
    println("\n\n")
  }

  object PartOne {
    def processCoordinate(action: ProcessCoordinateAction, grid: Array[Array[Int]], flashingSet: mutable.HashSet[(Int, Int)], row: Int, col: Int): Unit = {
      action match {
        case IncreaseEnergy =>
          val newEnergyLevel = grid(row)(col) + 1
          grid(row)(col) = newEnergyLevel

          if (!flashingSet.contains((row, col)) && newEnergyLevel > 9) {
            processCoordinate(Flash, grid, flashingSet, row, col)
          }
        case Flash =>
          flashingSet.add((row, col))
          // Upper-Left
          if (row - 1 >= 0 && col - 1 >= 0) {
            processCoordinate(IncreaseEnergy, grid, flashingSet, row - 1, col - 1)
          }
          // Top
          if (row - 1 >= 0) {
            processCoordinate(IncreaseEnergy, grid, flashingSet, row - 1, col)
          }
          // Upper-Right
          if (row - 1 >= 0 && col + 1 < LENGTH) {
            processCoordinate(IncreaseEnergy, grid, flashingSet, row - 1, col + 1)
          }
          // Left
          if (col - 1 >= 0) {
            processCoordinate(IncreaseEnergy, grid, flashingSet, row, col - 1)
          }
          // Right
          if (col + 1 < LENGTH) {
            processCoordinate(IncreaseEnergy, grid, flashingSet, row, col + 1)
          }
          // Lower-Left
          if (row + 1 < LENGTH && col - 1 >= 0) {
            processCoordinate(IncreaseEnergy, grid, flashingSet, row + 1, col - 1)
          }
          // Bottom
          if (row + 1 < LENGTH) {
            processCoordinate(IncreaseEnergy, grid, flashingSet, row + 1, col)
          }
          // Lower-Right
          if (row + 1 < LENGTH && col + 1 < LENGTH) {
            processCoordinate(IncreaseEnergy, grid, flashingSet, row + 1, col + 1)
          }
      }
    }

    def setFlashesToZero(grid: Array[Array[Int]], flashesLst: List[(Int, Int)]): Unit = {
      flashesLst.foreach {
        case (row, col) =>
          grid(row)(col) = 0
      }
    }

    def getFlashes(grid: Array[Array[Int]]): Int = {
      val flashingSet = mutable.HashSet[(Int, Int)]()
      var flashesCount = 0

      (1 to 100).foreach(_ => {
        (0 until LENGTH).foreach(row => {
          (0 until LENGTH).foreach(col => {
            processCoordinate(IncreaseEnergy, grid, flashingSet, row, col)
          })
        })

        flashesCount = flashesCount + flashingSet.size
        setFlashesToZero(grid, flashingSet.toList)
        flashingSet.clear()
      })

      flashesCount
    }
  }

  object PartTwo {

    def getFirstStepWithAllFlashing(grid: Array[Array[Int]]): Int = {
      val flashingSet = mutable.HashSet[(Int, Int)]()
      var step = 0

      while (flashingSet.size != 100) {
        flashingSet.clear()

        (0 until LENGTH).foreach(row => {
          (0 until LENGTH).foreach(col => {
            processCoordinate(IncreaseEnergy, grid, flashingSet, row, col)
          })
        })

        setFlashesToZero(grid, flashingSet.toList)
        step = step + 1
      }

      step
    }
  }

  def main(args: Array[String]): Unit = {
    /* Part One */
    println(getFlashes(parseData("day11input_example.txt")))
    println(getFlashes(parseData("day11input.txt")))
    /* Part Two */
    println(getFirstStepWithAllFlashing(parseData("day11input_example.txt")))
    println(getFirstStepWithAllFlashing(parseData("day11input.txt")))
  }
}
