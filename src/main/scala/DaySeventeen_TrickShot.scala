import DaySeventeen_TrickShot.PartOne.findMaxHeight
import DaySeventeen_TrickShot.PartTwo.getNumVelocities

object DaySeventeen_TrickShot {
  def parseData(filename: String): (Array[Int], Array[Int]) = {
    val bufferedSource = io.Source.fromInputStream(ClassLoader.getSystemResourceAsStream(filename))
    val targetCoords = bufferedSource.getLines().next().replaceAll("target area: ", "")
      .split(",").map(_.trim)
    bufferedSource.close()
    val xCoords = targetCoords(0).slice(2, targetCoords(0).length).split("[.][.]").map(_.toInt)
    val yCoords = targetCoords(1).slice(2, targetCoords(1).length).split("[.][.]").map(_.toInt)
    (xCoords, yCoords)
  }

  object PartOne {
    def findMaxHeight(coords: (Array[Int], Array[Int])): Int = {
      coords match {
        case (_, yCoords) =>
          (yCoords.min * ((yCoords.min + 1) / 2.0)).toInt
      }
    }
  }

  object PartTwo {
    def getCurrentPosition(initialVX: Int, initialVY: Int, currTime: Int): (Int, Int) = {
      val currentY = initialVY * currTime - (currTime - 1) * currTime / 2
      val currentX =
        if (currTime < initialVX) {
          (2 * initialVX - currTime + 1) * (currTime) / 2
        } else {
          initialVX * (initialVX + 1) / 2
        }
      (currentX, currentY)
    }

    def checkIntersect(vx: Int, vy: Int, upperBound: (Int, Int), lowerBound: (Int, Int)): Boolean = {
      val minTime = Math.floor(vy + Math.sqrt(vy * vy - 2 * upperBound._2)).toInt
      val maxTime = Math.floor(vy + Math.sqrt(vy * vy - 2 * lowerBound._2)).toInt

      (minTime to maxTime + 2).foldLeft(false)((result, currTime) => {
        getCurrentPosition(vx, vy, currTime) match {
          case (currentX, currentY) =>
            result || (
              (upperBound._1 <= currentX && currentX <= lowerBound._1) &&
                (lowerBound._2 <= currentY && currentY <= upperBound._2)
            )
        }
      })
    }

    def getNumVelocities(coords: (Array[Int], Array[Int])): Int = {
      coords match {
        case (xCoords, yCoords) =>
          val xCoords2 = (xCoords(0), xCoords(1))
          val yCoords2 = (yCoords(0), yCoords(1))
          val vyMin = yCoords2._1
          val vxMax = xCoords2._2

          val vyMax = -yCoords2._1
          val vxMin = Math.floor(Math.sqrt(2 * xCoords2._1) - 1).toInt

          // Upper-Left Corner of Area
          val upperBound = (xCoords2._1, yCoords2._2)
          // Bottom-Left Corner of Area
          val lowerBound = (xCoords2._2, yCoords2._1)

          var sum = 0
          (vxMin to vxMax + 1).foreach(vx => {
            (vyMin to vyMax + 1).foreach(vy => {
              if (checkIntersect(vx, vy, upperBound, lowerBound)) {
                sum = sum + 1
              }
            })
          })

          sum
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(findMaxHeight(parseData("day17input_example.txt")))
    println(findMaxHeight(parseData("day17input.txt")))
    println(getNumVelocities(parseData("day17input_example.txt")))
    println(getNumVelocities(parseData("day17input.txt")))
  }
}
