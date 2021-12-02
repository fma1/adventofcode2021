import java.io.InputStream

object Utils {
  val readFile: String => List[String] = (filename: String) => {
    val bufferedSource = io.Source.fromInputStream(ClassLoader.getSystemResourceAsStream(filename))
    val lines = (for (line <- bufferedSource.getLines()) yield line).toList
    bufferedSource.close
    lines
  }

  val readFileAsInts: String => List[Int] = (filename: String) => {
    val bufferedSource = io.Source.fromInputStream(ClassLoader.getSystemResourceAsStream(filename))
    val lines = (for (line <- bufferedSource.getLines()) yield line).toList.map(_.toInt)
    bufferedSource.close
    lines
  }
}
