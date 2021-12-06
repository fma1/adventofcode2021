import DayFive_HydrothermalVenture.PartOne.getNumberOfOverlappingPoints
import DayFive_HydrothermalVenture.PartTwo.getNumberOfOverlappingPointsWithDiagonals

import scala.collection.mutable

object DayFive_HydrothermalVenture {
  val IGNORE_DIAGONALS = true
  val DONT_IGNORE_DIAGONALS = false

  case class Coordinate(x: Int, y: Int)

  val updateCoordinate: (Coordinate, mutable.Map[Coordinate, Int]) => Unit =
    (coordinate, grid) => {
      // println(s"Updating $coordinate from ${grid(coordinate)} to ${grid(coordinate) + 1}")
      grid.update(coordinate, grid(coordinate) + 1)
    }

  val processLine: (Array[Array[Int]], mutable.Map[Coordinate, Int], Boolean) => Unit =
    (pairOfPoints, grid, ignoreDiagonals) => {
      var x1 = pairOfPoints(0)(0)
      var y1 = pairOfPoints(0)(1)
      var x2 = pairOfPoints(1)(0)
      var y2 = pairOfPoints(1)(1)
      // println(s"Processing ($x1, $y1) -> ($x2, $y2)")

      if (x1 == x2) {
        if (y1 > y2) {
          val temp = y1
          y1 = y2
          y2 = temp
        }

        (y1 to y2).foreach(y => {
          val coordinate = Coordinate(x1, y)
          updateCoordinate(coordinate, grid)
        })
      } else if (y1 == y2) {
        if (x1 > x2) {
          val temp = x1
          x1 = x2
          x2 = temp
        }

        (x1 to x2).foreach(x => {
          val coordinate = Coordinate(x, y1)
          updateCoordinate(coordinate, grid)
        })
      } else {
        if (ignoreDiagonals) {
          ()
        } else {
          var coordinate1 = Coordinate(x1, y1)
          var coordinate2 = Coordinate(x2, y2)

          // Swap for consistency of always x1 < x2
          if (x1 > x2) {
            var temp = coordinate1
            coordinate1 = coordinate2
            coordinate2 = temp
          }

          coordinate1 match {
            case Coordinate(x1, y1) =>
              coordinate2 match {
                case Coordinate(x2, y2) =>
                  val xRange = x1 to x2
                  val yRange = if (y1 < y2) y1 to y2 else y1 to y2 by -1
                  val range =
                     xRange zip yRange

                  range.map(tup => Coordinate(tup._1, tup._2))
                    .foreach(coord => updateCoordinate(coord, grid))
              }
          }
        }
      }
    }

  def getLines(filename: String): List[Array[Array[Int]]] = {
    val bufferedSource = io.Source.fromInputStream(ClassLoader.getSystemResourceAsStream(filename))
    val linesLst = bufferedSource.getLines()
      .map(_.split("->").map(_.split(",").map(_.trim.toInt)))
      .toList
    bufferedSource.close()
    linesLst
  }

  object PartOne {
    def getNumberOfOverlappingPoints(filename: String): Int = {
      val grid = mutable.HashMap[Coordinate, Int]().withDefaultValue(0)
      val linesLst = getLines(filename)
      linesLst.foreach(line => processLine(line, grid, IGNORE_DIAGONALS))
      grid.count(_._2 > 1)
    }
  }

  object PartTwo {
    def getNumberOfOverlappingPointsWithDiagonals(filename: String): Int = {
      val grid = mutable.HashMap[Coordinate, Int]().withDefaultValue(0)
      val linesLst = getLines(filename)
      linesLst.foreach(line => processLine(line, grid, DONT_IGNORE_DIAGONALS))
      // println(s"List of coordinates > 1: ${grid.filter(_._2 > 1).mkString(",")}")
      grid.count(_._2 > 1)
    }
  }

  def main(args: Array[String]): Unit = {
    /* Part One */
    // println(getNumberOfOverlappingPoints("day5input_example.txt"))
    println(getNumberOfOverlappingPoints("day5input.txt"))
    /* Part Two */
    // println(getNumberOfOverlappingPointsWithDiagonals("day5input_example.txt"))
    println(getNumberOfOverlappingPointsWithDiagonals("day5input.txt"))
  }
}
