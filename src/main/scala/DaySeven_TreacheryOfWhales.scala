object DaySeven_TreacheryOfWhales {

  def parseData(filename: String): Array[Int] = {
    val bufferedSource = io.Source.fromInputStream(ClassLoader.getSystemResourceAsStream(filename))
    val numbersStr = bufferedSource.getLines().next()
    bufferedSource.close
    numbersStr.split(",").map(_.trim.toInt)
  }

  object PartOne {
    def getMinFuel(positions: Array[Int]): Int = {
      val median = positions.sorted.apply(positions.length / 2)
      positions.foldLeft(0)((acc, curr) => acc + Math.abs(curr - median))
    }
  }

  object PartTwo {
    def naturalSum(n: Int): Int = {
      (n to 0 by -1).sum
    }

    def getMinFuel(positions: Array[Int]): Int = {
      val average = Math.floor(positions.sum.toDouble / positions.length.toDouble).toInt
      positions.foldLeft(0)((acc, curr) => {
        println(s"Move from ${curr} to ${average}: ${naturalSum(Math.abs(curr - average))}")
        acc + naturalSum(Math.abs(curr - average))
      })
    }
  }

  def main(args: Array[String]): Unit = {
    /* Part One */
    println(PartOne.getMinFuel(parseData("day7input_example.txt")))
    println(PartOne.getMinFuel(parseData("day7input.txt")))

    /* Part Two */
    println(PartTwo.getMinFuel(parseData("day7input_example.txt")))
    println(PartTwo.getMinFuel(parseData("day7input.txt")))
  }
}
