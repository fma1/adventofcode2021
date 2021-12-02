import Utils.readFile

object DayTwo_Dive {
  object PartOne {
    def getHorizPosTimesDepth(seq: Seq[String]): Int = {
      val horizPosAndDepthTuple =
        seq.foldLeft((0, 0))((acc: (Int, Int), curr: String) => {
          val currSplitArr = curr.split(' ')
          val directionFirstChar = currSplitArr(0)(0)
          val x = currSplitArr(1).toInt

          acc match {
            case (position, depth) =>
              directionFirstChar match {
                case 'f' =>
                  (position + x, depth)
                case 'u' =>
                  (position, depth - x)
                case 'd' =>
                  (position, depth + x)
              }
          }
        })
      horizPosAndDepthTuple._1 * horizPosAndDepthTuple._2
    }
  }

  object PartTwo {
    def getHorizPosTimesDepth(seq: Seq[String]): Int = {
      val horizPosAndDepthTuple =
        seq.foldLeft((0, 0, 0))((acc: (Int, Int, Int), curr: String) => {
          val currSplitArr = curr.split(' ')
          val directionFirstChar = currSplitArr(0)(0)
          val x = currSplitArr(1).toInt

          acc match {
            case (position, depth, aim) =>
              directionFirstChar match {
                case 'f' =>
                  (position + x, depth + aim * x, aim)
                case 'u' =>
                  (position, depth, aim - x)
                case 'd' =>
                  (position, depth, aim + x)
              }
          }
        })
      horizPosAndDepthTuple._1 * horizPosAndDepthTuple._2
    }
  }

  def main(args: Array[String]): Unit = {
    val exampleCourse = List(
      "forward 5",
      "down 5",
      "forward 8",
      "up 3",
      "down 8",
      "forward 2"
    )

    /* Part One */
    println(
      PartOne.getHorizPosTimesDepth(
        exampleCourse))

    println(
      PartOne.getHorizPosTimesDepth(
        readFile("day2input.txt")))

    /* Part Two */
    println(
      PartTwo.getHorizPosTimesDepth(
        exampleCourse))

    println(
      PartTwo.getHorizPosTimesDepth(
        readFile("day2input.txt")))
  }
}
