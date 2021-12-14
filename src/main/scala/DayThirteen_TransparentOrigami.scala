import DayThirteen_TransparentOrigami.PartOne.getNumVisibleDotsAfterFoldOne
import DayThirteen_TransparentOrigami.PartTwo.{foldPaperAllTimes, printGrid}

import scala.collection.mutable

object DayThirteen_TransparentOrigami {
  type OrigamiData = (List[(Int, Int)], List[(Char, Int)])

  def parseData(filename: String): OrigamiData = {
    val bufferedSource = io.Source.fromInputStream(ClassLoader.getSystemResourceAsStream(filename))
    val lines = bufferedSource.getLines().toArray
    val initialValue = (List[(Int, Int)](), List[(Char, Int)]())
    bufferedSource.close()
    lines.foldLeft(initialValue)((acc, line) => {
      acc match {
        case (coordLst, foldLst) =>
          if (line.contains(",")) {
            val coordAry = line.trim.split(",").map(_.toInt)
            ((coordAry(0), coordAry(1)) +: coordLst, foldLst)
          } else if (line.isEmpty) {
            acc
          } else {
            val words = line.split(" ")
            val foldAry = words(2).split("=")
            val newFold = (foldAry(0)(0), foldAry(1).trim.toInt)

            (coordLst, newFold +: foldLst)
          }
      }
    })
  }

  def foldPaper(fold: (Char, Int), points: mutable.HashSet[(Int, Int)]): Unit = {
    val coordLst = points.toList

    fold match {
      case (axis, foldLineNum) =>
        axis match {
          case 'x' =>
            coordLst.foreach {
              case (x, y) =>
                if (x > foldLineNum) {
                  points.add((foldLineNum - (x - foldLineNum), y))
                  points.remove((x, y))
                }
            }
            coordLst.filter(_._1 == foldLineNum).foreach(points.remove)
          case 'y' =>
            coordLst.foreach {
              case (x, y) =>
                if (y > foldLineNum) {
                  points.add((x, foldLineNum - (y - foldLineNum)))
                  points.remove((x, y))
                }
            }
            coordLst.filter(_._2 == foldLineNum).foreach(points.remove)
        }
    }
  }

  object PartOne {
    def getNumVisibleDotsAfterFoldOne(data: OrigamiData): Int = {
      val points = mutable.HashSet[(Int, Int)]()
      data match {
        case (coordLst, foldLst) =>
          coordLst.foreach(points.add)
          foldPaper(foldLst.reverse.head, points)
      }

      points.size
    }
  }

  object PartTwo {
    def foldPaperAllTimes(data: OrigamiData): mutable.HashSet[(Int, Int)] = {
      val points = mutable.HashSet[(Int, Int)]()
      data match {
        case (coordLst, foldLst) =>
          coordLst.foreach(points.add)
          foldLst.reverse.foreach(fold => foldPaper(fold, points))
      }
      points
    }

    def printGrid(points: mutable.HashSet[(Int, Int)], maxX: Int, maxY: Int, foldTimes: Int): Unit = {
      (0 until (maxX/(2*foldTimes))).foreach(row => {
        (0 until (maxY/(2*foldTimes))).foreach(col => {
          if (points.contains((col, row))) {
            print("#")
          } else {
            print(".")
          }
        })
        println()
      })
    }
  }

  def main(args: Array[String]): Unit = {
    // println(getNumVisibleDotsAfterFoldOne(parseData("day13input_example.txt")))
    // println(getNumVisibleDotsAfterFoldOne(parseData("day13input.txt")))
    val data = parseData("day13input.txt")
    val initialMaxY = data._1.map(_._1).max
    val initialMaxX = data._1.map(_._2).max
    val maxY = if (initialMaxX % 2 != 0) initialMaxX + 1 else initialMaxX
    val maxX = if (initialMaxY % 2 != 0) initialMaxY + 1 else initialMaxY
    val points = foldPaperAllTimes(data)
    printGrid(points, maxX, maxY, data._2.size)
  }
}
