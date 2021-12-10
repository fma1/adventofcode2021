import DayNine_SmokeBasin.PartOne.{getLowPoints, getRiskLevel}
import DayNine_SmokeBasin.PartTwo.getMultipleOfThreeLargestSizes

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object DayNine_SmokeBasin {
  def parseData(filename: String): ArrayBuffer[Array[Int]] = {
    val bufferedSource = io.Source.fromInputStream(ClassLoader.getSystemResourceAsStream(filename))
    val arrayOfArrays = ArrayBuffer[Array[Int]]()
    bufferedSource.getLines().foreach(line => arrayOfArrays += line.trim.split("").map(_.toInt))
    bufferedSource.close()
    arrayOfArrays
  }

  object PartOne {
    def getLowPoints(grid: ArrayBuffer[Array[Int]]): ArrayBuffer[(Int, Int, Int)] = {
      val possibleLowPointsMap = mutable.HashMap[Int, ArrayBuffer[Int]]()
      val lowPoints = ArrayBuffer[(Int, Int, Int)]()

      grid.zipWithIndex.foreach {
        case (rowOfPoints, rowIdx) =>
          var prevHeight = Int.MaxValue
          rowOfPoints.zipWithIndex.foreach {
            case (height, colIdx) =>
              val possibleLowPoints =
                possibleLowPointsMap.getOrElseUpdate(rowIdx, ArrayBuffer[Int]())
              val nextHeight = if (rowOfPoints.length > colIdx + 1) rowOfPoints(colIdx + 1) else Int.MaxValue
              if (height < prevHeight && height < nextHeight) {
                possibleLowPoints += colIdx
              }
              prevHeight = height
          }
      }

      possibleLowPointsMap.foreach {
        case (rowIdx, possibleLowPoints) =>
          possibleLowPoints.foreach(colIdx => {
            val prevRowHeight = if (rowIdx - 1 >= 0) grid(rowIdx - 1)(colIdx) else Int.MaxValue
            val nextRowHeight = if (rowIdx + 1 < grid.length) grid(rowIdx + 1)(colIdx) else Int.MaxValue
            val currHeight = grid(rowIdx)(colIdx)
            if (currHeight < prevRowHeight && currHeight < nextRowHeight) {
              lowPoints.append((currHeight, rowIdx, colIdx))
            }
          })
      }
      lowPoints
    }

    def getRiskLevel(grid: ArrayBuffer[Array[Int]]): Int = {
      getLowPoints(grid).map(_._1 + 1).sum
    }
  }

  object PartTwo {
    def getMultipleOfThreeLargestSizes(grid: ArrayBuffer[Array[Int]], lowPoints: ArrayBuffer[(Int, Int, Int)]): Int = {
      val lowPointToBasinSize = mutable.HashMap[(Int, Int, Int), Int]()
      val queue = mutable.Queue[(Int, Int, Int)]()
      lowPoints.foreach {
        case (height, rowIdx, colIdx) =>
          var counter = 0
          queue.clear()
          queue.enqueue((height, rowIdx, colIdx))
          val seenPoints = mutable.HashSet[(Int, Int)]()

          while (queue.nonEmpty) {
            queue.dequeue() match {
              case (height, rowIdx, colIdx) =>
                if (height == 9) {
                  ()
                } else {
                  if (rowIdx - 1 >= 0 && grid(rowIdx - 1)(colIdx) > height) {
                    val point = (rowIdx - 1, colIdx)
                    if (!seenPoints.contains(point)) {
                      queue.enqueue((grid(rowIdx - 1)(colIdx), rowIdx - 1, colIdx))
                      seenPoints += point
                    }
                  }
                  if (rowIdx + 1 < grid.length && grid(rowIdx + 1)(colIdx) > height) {
                    val point = (rowIdx + 1, colIdx)
                    if (!seenPoints.contains(point)) {
                      queue.enqueue((grid(rowIdx + 1)(colIdx), rowIdx + 1, colIdx))
                      seenPoints += point
                    }
                  }
                  if (colIdx - 1 >= 0 && grid(rowIdx)(colIdx - 1) > height) {
                    val point = (rowIdx, colIdx - 1)
                    if (!seenPoints.contains(point)) {
                      queue.enqueue((grid(rowIdx)(colIdx - 1), rowIdx, colIdx - 1))
                      seenPoints += point
                    }
                  }
                  if (colIdx + 1 < grid(rowIdx).length && grid(rowIdx)(colIdx + 1) > height) {
                    val point = (rowIdx, colIdx + 1)
                    if (!seenPoints.contains(point)) {
                      queue.enqueue((grid(rowIdx)(colIdx + 1), rowIdx, colIdx + 1))
                      seenPoints += point
                    }
                  }
                  counter = counter + 1
                }
            }
          }

          lowPointToBasinSize += (height, rowIdx, colIdx) -> counter
      }

      val sortedSizes = lowPointToBasinSize.values.toArray.sorted.reverse
      println(sortedSizes.mkString(","))
      sortedSizes(0) * sortedSizes(1) * sortedSizes(2)
    }
  }

  def main(args: Array[String]): Unit = {
    /* Part One */
    println(getRiskLevel(parseData("day9input_example.txt")))
    println(getRiskLevel(parseData("day9input.txt")))

    /* Part Two */
    val exampleGrid = parseData("day9input_example.txt")
    println(getMultipleOfThreeLargestSizes(exampleGrid, getLowPoints(exampleGrid)))
    val realGrid = parseData("day9input.txt")
    println(getMultipleOfThreeLargestSizes(realGrid, getLowPoints(realGrid)))
  }
}
