import java.io.InputStream

object DayOne {
  type AccPrevType = (Int, Some[Int])

  object SonarSweep {
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

    val readFile: InputStream => List[Int] = (is: InputStream) => {
      val bufferedSource = io.Source.fromInputStream(is)
      val lines = (for (line <- bufferedSource.getLines()) yield line).toList.map(_.toInt)
      bufferedSource.close
      lines
    }

    def main(args: Array[String]): Unit = {
      println(sonarSweep(readFile(ClassLoader.getSystemResourceAsStream("day1input.txt"))))
    }
  }
}
