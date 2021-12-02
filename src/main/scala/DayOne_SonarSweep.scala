import DayOne_SonarSweep.PartOne.{readFile, sonarSweep}
import DayOne_SonarSweep.PartTwo.getThreeWindowSums

import java.io.InputStream
import scala.annotation.tailrec

object DayOne_SonarSweep {
  type AccPrevType = (Int, Some[Int])

  object PartOne {
    val readFile: InputStream => List[Int] = (is: InputStream) => {
      val bufferedSource = io.Source.fromInputStream(is)
      val lines = (for (line <- bufferedSource.getLines()) yield line).toList.map(_.toInt)
      bufferedSource.close
      lines
    }

    val sonarSweep: Seq[Int] => Int = (depths: Seq[Int]) => {
      val initialVal: (Int, Option[Int]) = (0, None)
      depths.foldLeft(initialVal)((acc: (Int, Option[Int]), curr: Int) => {
        acc match {
          case (intAcc, prevOpt) =>
            val currOpt = Some(curr)
            prevOpt match {
              case Some(prev) =>
                if (curr > prev) (intAcc + 1, currOpt) else (intAcc, currOpt)
              case None =>
                (0, currOpt)
            }
        }
      })._1
    }

    /*
    def main(args: Array[String]): Unit = {
    }

     */
  }

  object PartTwo {
    def getThreeWindowSumsOld(ary: Array[Int]): List[Int] = {
      @tailrec
      def outerLoop(idx: Int, lst: List[Int]): List[Int] = {
        @tailrec
        def innerLoop(idx: Int, lst: List[Int], prevResult: Int, counter: Int = 0): List[Int] = {
          if (counter == 4) {
            lst
          } else {
            val curr = ary(idx)
            val result =
              if (idx % 4 == 0) {
                curr + ary(idx + 1) + ary(idx + 2)
              } else {
                prevResult + ary(idx + 2)
              }
            innerLoop(idx + 1, result +: lst, result - curr, counter + 1)
          }
        }

        if (idx < ary.length - 4) {
          outerLoop(idx + 4, innerLoop(idx, lst, -1, 0))
        } else {
          lst
        }
      }

      outerLoop(0, List[Int]()).reverse
    }

    def getThreeWindowSums(ary: Array[Int]): List[Int] = {
      @tailrec
      def outerLoop(idx: Int, lst: List[Int], prevResult: Int = -1): List[Int] = {
        if (idx == ary.length - 2) {
          lst
        } else {
          val curr = ary(idx)
          val result =
            if (prevResult == -1) {
              curr + ary(idx + 1) + ary(idx + 2)
            } else {
              prevResult + ary(idx + 2)
            }
          outerLoop(idx + 1, result +: lst, result - curr)
        }
      }

      outerLoop(0, List[Int]()).reverse
    }
  }

  def main(args: Array[String]): Unit = {
    /* Part One */
    println(sonarSweep(readFile(ClassLoader.getSystemResourceAsStream("day1input.txt"))))

    /* Part Two */
    //val linesAry = Array(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
    val linesAry = readFile(ClassLoader.getSystemResourceAsStream("day1input.txt")).toArray
    val threeWindowSums = getThreeWindowSums(linesAry)
    println(sonarSweep(threeWindowSums))
  }
}
