/**
 * Represents square with numbers in the corners
 *
 * @param leftUp number in left upper corner
 * @param rightUp number in right upper corner
 * @param leftDown number in left lower corner
 * @param rightDown number in right lower corner
 */
case class Square(leftUp: Int, rightUp: Int, leftDown: Int, rightDown: Int) {
  def getUpperLine: String = {
    leftUp + " " + rightUp
  }

  def getLowerLine: String = {
    leftDown + " " + rightDown
  }

  override def toString: String = {
    s"$leftUp $rightUp $leftDown $rightDown"
  }
}

/**
 * Solver for magic squares
 */
object MagicSquares {
  private val neutralSquare = Square(0, 0, 0, 0)
  private val maxValue = 10

  /**
   * Validators for board cells
   * Use it in the right order: every validator requires some cells to be valid
   * Order: 3, 4, 7, 8, 0, 1, 2, 6, 5, 9, 10, 11
   */
  private val validators: Vector[Map[Int, Square] => Boolean] = Vector(
    board => validateVerticalSquares(board(0), board(3)),
    board => validateSquares(board(0), board(1), board(3), board(4)),
    board =>
      validateHorizontalSquares(board(2), board(3)) &&
        validateTriangle(neutralSquare, board(0), board(2), board(3)),
    board => validateSquare(board(3)),
    board => validateHorizontalSquares(board(3), board(4)),
    board =>
      validateHorizontalSquares(board(4), board(5)) &&
        validateTriangle(board(1), neutralSquare, board(4), board(5)),
    board => validateSquares(board(2), board(3), board(6), board(7)),
    board => validateVerticalSquares(board(3), board(7)),
    board => validateSquares(board(3), board(4), board(7), board(8)),
    board => validateSquares(board(4), board(5), board(8), board(9)),
    board =>
      validateVerticalSquares(board(7), board(10)) &&
        validateTriangle(board(6), board(7), neutralSquare, board(10)),
    board =>
      validateSquares(board(7), board(8), board(10), board(11)) &&
        validateTriangle(board(8), board(9), board(11), neutralSquare)
  )

  /**
   * Prints all solutions for magic squares separated by an empty line
   *
   * @param squares squares to be placed on the board
   */
  def printSolutions(squares: Vector[Square]): Unit = {
    val freeBoardIds: Vector[Int] = Vector(3, 4, 7, 8, 0, 1, 2, 6, 5, 9, 10, 11)
    val freeIds: Set[Int] = (0 until 12).toSet
    val board: Map[Int, Square] = Map()
    assignSquares(board, freeBoardIds, freeIds, squares)
  }

  /**
   * Places squares on the board and prints the result
   *
   * @param board board
   * @param freeBoardIds indexes of free board cells
   * @param freeIds indexes of unused squares
   * @param squares squares
   */
  private def assignSquares(board: Map[Int, Square], freeBoardIds: Vector[Int], freeIds: Set[Int],
                    squares: Vector[Square]): Unit = freeBoardIds match {
    case boardId +: boardIds =>
      for (
        id <- freeIds;
        newBoard = board + (boardId -> squares(id))
        if validators(boardId)(newBoard)
      ) {
        assignSquares(newBoard, boardIds, freeIds - id, squares)
      }
    case _ => printBoard(board)
  }

  /**
   * Validates a single square
   *
   * @param square square to validate
   * @return whether square is valid
   */
  private def validateSquare(square: Square): Boolean = {
    square.leftUp <= maxValue && square.rightUp <= maxValue &&
      square.leftDown <= maxValue && square.rightDown <= maxValue
  }

  /**
   * Validates two squares to be placed horizontally
   *
   * @param square1 left square
   * @param square2 right square
   * @return whether squares are valid
   */
  private def validateHorizontalSquares(square1: Square, square2: Square): Boolean = {
    validateSquare(square1) && validateSquare(square2) &&
      square1.rightUp + square2.leftUp <= maxValue &&
      square1.rightDown + square2.leftDown <= maxValue
  }

  /**
   * Validates two squares to be placed vertically
   *
   * @param square1 upper square
   * @param square2 lower square
   * @return whether squares are valid
   */
  private def validateVerticalSquares(square1: Square, square2: Square): Boolean = {
    validateSquare(square1) && validateSquare(square2) &&
      square1.leftDown + square2.leftUp <= maxValue &&
      square1.rightDown + square2.rightUp <= maxValue
  }

  /**
   * Validates four squares to be placed in the form of a square
   *
   * @param square1 left upper square
   * @param square2 right upper square
   * @param square3 left lower square
   * @param square4 right lower square
   * @return whether squares are valid
   */
  private def validateSquares(square1: Square, square2: Square, square3: Square, square4: Square): Boolean = {
    validateHorizontalSquares(square1, square2) && validateHorizontalSquares(square3, square4) &&
      validateVerticalSquares(square1, square3) && validateVerticalSquares(square2, square4) &&
      square1.rightDown + square2.leftDown + square3.rightUp + square4.leftUp == maxValue
  }

  /**
   * Validates joining squares in the form of a triangle
   * One of squares is neutral
   *
   * @param square1 left upper square
   * @param square2 right upper square
   * @param square3 left lower square
   * @param square4 right lower square
   * @return whether squares are valid
   */
  private def validateTriangle(square1: Square, square2: Square, square3: Square, square4: Square): Boolean = {
    square1.rightDown + square2.leftDown + square3.rightUp + square4.leftUp <= maxValue
  }

  private def printFunctionMap(function: Square => String, squares: Seq[Square]): Unit = {
    println(f"${squares.map(function).mkString(" ")}%11s")
  }

  private def printBoardLine(squares: Square*): Unit = {
    printFunctionMap(_.getUpperLine, squares)
    printFunctionMap(_.getLowerLine, squares)
  }

  private def printBoard(board: Map[Int, Square]): Unit = {
    printBoardLine(board(0), board(1))
    printBoardLine(board(2), board(3), board(4), board(5))
    printBoardLine(board(6), board(7), board(8), board(9))
    printBoardLine(board(10), board(11))
    println()
  }

  private def printBoardAsSequence(board: Map[Int, Square]): Unit = {
    (0 until 12).foreach(id => println(board(id).toString))
  }
}