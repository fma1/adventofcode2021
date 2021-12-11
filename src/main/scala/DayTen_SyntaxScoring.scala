import DayTen_SyntaxScoring.PartOne.{getHighScoreForSyntaxErrors, partOneMappingFunc}
import DayTen_SyntaxScoring.PartTwo.getHighScoreForAutoComplete

import scala.collection.mutable

object DayTen_SyntaxScoring {
  val BIG_INT_ZERO = BigInt(0)
  val partOneScoringRubric = Map(')' -> BigInt(3), ']' -> BigInt(57), '}' -> BigInt(1197), '>' -> BigInt(25137))
  val partTwoScoringRubric = Map(')' -> BigInt(1), ']' -> BigInt(2), '}' -> BigInt(3), '>' -> BigInt(4))
  val operandOpposites = Map(')' -> '(', ']' -> '[', '}' -> '{', '>' -> '<',
    '(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')

  def parseData(filename: String): Array[String] = {
    val bufferedSource = io.Source.fromInputStream(ClassLoader.getSystemResourceAsStream(filename))
    val lines = bufferedSource.getLines().toArray
    bufferedSource.close()
    lines
  }

  object PartOne {
    val partOneMappingFunc: String => BigInt = line => {
      val stack = mutable.Stack[Char]()
      line.foldLeft(BIG_INT_ZERO)((acc, char) => {
        if (acc.equals(BIG_INT_ZERO)) {
          char match {
            case ')' | ']' | '}' | '>' =>
              if (stack.pop() != operandOpposites(char)) {
                partOneScoringRubric(char)
              } else {
                acc
              }
            case _ =>
              stack.push(char)
              acc
          }
        } else {
          acc
        }
      })
    }

    def getHighScoreForSyntaxErrors(lines: Array[String]): BigInt = {
      lines.map(partOneMappingFunc).sum
    }
  }

  object PartTwo {
    val partTwoMappingFunc: String => BigInt = line => {
      val newLine = line.foldLeft(List[Char]())((acc, char) => {
        char match {
          case ')' | ']' | '}' | '>' =>
            acc.tail
          case char =>
            char +: acc
        }
      })
      newLine.foldLeft(BIG_INT_ZERO)((acc, char) => {
        BigInt(5) * acc + partTwoScoringRubric(operandOpposites(char))
      })
    }

    def getHighScoreForAutoComplete(lines: Array[String]): BigInt = {
      val zippedPartOnePartTwoScores = lines.map(partOneMappingFunc) zip lines.map(partTwoMappingFunc)
      println(lines.map(partTwoMappingFunc).mkString(","))
      val sortedHighScores = zippedPartOnePartTwoScores
        .filter(_._1.equals(BIG_INT_ZERO))
        .map(_._2)
        .sorted
      sortedHighScores(sortedHighScores.length / 2)
    }
  }

  def main(args: Array[String]): Unit = {
    /* Part One */
    println(getHighScoreForSyntaxErrors(parseData("day10input_example.txt")))
    println(getHighScoreForSyntaxErrors(parseData("day10input.txt")))

    /* Part Two */
    println(getHighScoreForAutoComplete(parseData("day10input_example.txt")))
    println(getHighScoreForAutoComplete(parseData("day10input.txt")))
  }
}
