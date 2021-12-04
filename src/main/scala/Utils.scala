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

  val readFileAsBinaryNumbers: String => List[Int] = (filename: String) => {
    val bufferedSource = io.Source.fromInputStream(ClassLoader.getSystemResourceAsStream(filename))
    val lines = (for (line <- bufferedSource.getLines()) yield line).toList.map(x => Integer.parseInt(x, 2))
    bufferedSource.close
    lines
  }
}
