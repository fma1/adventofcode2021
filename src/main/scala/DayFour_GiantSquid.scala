import DayFour_GiantSquid.PartOne.{getFinalScoreForFirstWin, getFinalScoreWithWinningPair, getNumbersAndBoardsWithMaps}
import DayFour_GiantSquid.PartTwo.getFinalScoreForLastWin

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object DayFour_GiantSquid {
  val ROWS = 5
  val COLS = 5

  case class Coordinate(x: Int, y: Int)

  case class SquareTuple(value: Int, coordinate: Coordinate, var wasCalled: Boolean = false)

  case class WinningCombTuple(found: Boolean = false, winningNumber: Int = -1, winningBoard: Array[Array[SquareTuple]] = null)

  def hasCompleteColumnOrRow(board: Array[Array[SquareTuple]], x: Int, y: Int): Boolean = {
    (0 to 4).forall(curr => board(x)(curr).wasCalled) ||
      (0 to 4).forall(curr => board(curr)(y).wasCalled)
  }

  def hasCompleteColumnOrRowWithSets(board: Array[Array[SquareTuple]], boardSetX: mutable.HashSet[Int], boardSetY: mutable.HashSet[Int], x: Int, y: Int): Boolean = {
    val xPred = !boardSetX.contains(x) && (0 to 4).forall(curr => board(x)(curr).wasCalled)
    val yPred = !boardSetY.contains(y) && (0 to 4).forall(curr => board(curr)(y).wasCalled)
    if (xPred) {
      boardSetX.add(x)
    }
    if (yPred) {
      boardSetY.add(y)
    }
    xPred || yPred
  }

  object PartOne {
    def getNumbersAndBoardsWithMaps(filename: String): (Array[Int], ArrayBuffer[(Array[Array[SquareTuple]], mutable.HashMap[Int, Coordinate])]) = {
      val bufferedSource = io.Source.fromInputStream(ClassLoader.getSystemResourceAsStream(filename))
      val iterator = bufferedSource.getLines().filter(_.nonEmpty)

      val numbers = iterator.next().split(",").map(_.toInt)
      val boards = ArrayBuffer[Array[Array[SquareTuple]]]()
      val boardMaps = ArrayBuffer[mutable.HashMap[Int, Coordinate]]()

      // Parsing Logic
      var currArray: Array[Array[SquareTuple]] = null
      var boardMap: mutable.HashMap[Int, Coordinate] = null
      var rowIdx = 0
      iterator.zipWithIndex.foreach({
        case (line, lineIdx) =>
          val remainder = lineIdx % 5
          if (remainder == 0) {
            currArray = Array.ofDim[SquareTuple](ROWS, COLS)
            boardMap = mutable.HashMap[Int, Coordinate]()
            rowIdx = 0
          }

          val nums = line.trim.split("\\s+")
            .map(str => Integer.parseInt(str))
            .zipWithIndex
            .map { case (num, colIdx) => SquareTuple(num, Coordinate(rowIdx, colIdx)) }

          nums.foreach({
            case SquareTuple(value, coordinate, _) =>
              boardMap += (value -> coordinate)
          })

          Array.copy(nums, 0, currArray(rowIdx), 0, nums.length)

          if (remainder == 4) {
            boards += currArray
            boardMaps += boardMap
          }

          rowIdx = rowIdx + 1
      })
      bufferedSource.close

      val boardsWithMaps = boards zip boardMaps
      (numbers, boardsWithMaps)
    }

    def getFinalScoreForFirstWin(numbers: Array[Int], boardsWithMaps: ArrayBuffer[(Array[Array[SquareTuple]], mutable.HashMap[Int, Coordinate])]): Int = {
      val winningCombTuple =
        numbers.foldLeft(WinningCombTuple())((acc, currNum) => {
          acc match {
            case WinningCombTuple(found, _, _) =>
              if (!found) {
                boardsWithMaps.foldLeft(WinningCombTuple())((acc, boardWithMap) => {
                  acc match {
                    case WinningCombTuple(found, _, _) =>
                      if (!found) {
                        boardWithMap match {
                          case (board, boardMap) =>
                            boardMap.get(currNum) match {
                              case Some(Coordinate(x, y)) =>
                                val squareTuple = board(x)(y)
                                squareTuple.wasCalled = true

                                if (hasCompleteColumnOrRow(board, x, y)) {
                                  WinningCombTuple(found = true, currNum, board)
                                } else {
                                  acc
                                }
                              case None =>
                                acc
                            }
                        }
                      } else {
                        acc
                      }
                  }
                })
              } else {
                acc
              }
          }
        })


      winningCombTuple match {
        case WinningCombTuple(_, winningNumber, winningBoard) =>
          getFinalScoreWithWinningPair(winningNumber, winningBoard)
      }
    }

    def getFinalScoreWithWinningPair(winningNumber: Int, winningBoard: Array[Array[SquareTuple]]): Int = {
      val unmarkedSum =
        winningBoard.flatten
          .filterNot(_.wasCalled)
          .map(_.value)
          .sum
      println(s"unmarkedSum: $unmarkedSum")
      println(s"winningNumber: $winningNumber")
      unmarkedSum * winningNumber
    }
  }

  object PartTwo {
    def getFinalScoreForLastWin(numbers: Array[Int], boardsWithMaps: ArrayBuffer[(Array[Array[SquareTuple]], mutable.HashMap[Int, Coordinate])]): Int = {
      val winningPairs = ArrayBuffer[(Int, Int)]()
      val boardToSetX = mutable.HashMap[Array[Array[SquareTuple]], mutable.HashSet[Int]]()
      val boardToSetY = mutable.HashMap[Array[Array[SquareTuple]], mutable.HashSet[Int]]()

      boardsWithMaps.map(_._1).foreach(board => {
        boardToSetX += board -> mutable.HashSet[Int]()
        boardToSetY += board -> mutable.HashSet[Int]()
      })

      numbers.foreach(number => {
        boardsWithMaps.foreach {
          case (board, boardMap) =>
            boardMap.get(number) match {
              case Some(Coordinate(x, y)) =>
                val squareTuple = board(x)(y)
                squareTuple.wasCalled = true

                val setX = boardToSetX.get(board).head
                val setY = boardToSetY.get(board).head
                if (setX.isEmpty && setY.isEmpty && hasCompleteColumnOrRowWithSets(board, setX, setY, x, y)) {
                  val unmarkedSum =
                    board.flatten
                      .filterNot(_.wasCalled)
                      .map(_.value)
                      .sum
                  winningPairs.append((number, unmarkedSum))
                }
              case None =>
                ()
            }
        }
      })

      val winningPair = winningPairs(winningPairs.size - 1)
      winningPair._1 * winningPair._2
    }
  }

  def main(args: Array[String]): Unit = {
    var numbersAndBoardsWithMaps: (Array[Int], ArrayBuffer[(Array[Array[SquareTuple]], mutable.HashMap[Int, Coordinate])]) = null
    // Part One
    //numbersAndBoardsWithMaps = getNumbersAndBoardsWithMaps("day4input_example.txt")
    //numbersAndBoardsWithMaps = getNumbersAndBoardsWithMaps("day4input.txt")
    /*
    println(getFinalScoreForFirstWin(numbersAndBoardsWithMaps._1, numbersAndBoardsWithMaps._2))
     */

    // Part Two
    //numbersAndBoardsWithMaps = getNumbersAndBoardsWithMaps("day4input_example.txt")
    numbersAndBoardsWithMaps = getNumbersAndBoardsWithMaps("day4input.txt")
    println(getFinalScoreForLastWin(numbersAndBoardsWithMaps._1, numbersAndBoardsWithMaps._2))
  }
}
