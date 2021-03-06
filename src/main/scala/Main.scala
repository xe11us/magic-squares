import scala.io.StdIn.readLine

object Main extends App {
  print("Type file name: ")
  val file = readLine()

  try {
    val source = scala.io.Source.fromFile(file)
    try {
      val lines = source.getLines()
      val squares = lines.map(line => {
        val parsedLine = line.split("\\s+").map(_.toInt)
        parsedLine match {
          case Array(a, b, c, d) => Square(a, b, c, d)
          case _ => throw new IllegalArgumentException("Expected four integers")
        }
      }).toVector

      if (squares.size != 12) {
        throw new IllegalArgumentException("Expected 12 squares")
      }

      MagicSquares.printSolutions(squares)
    } finally {
      source.close()
    }
  } catch {
    case e: Exception => println(e.getMessage)
  }
}
