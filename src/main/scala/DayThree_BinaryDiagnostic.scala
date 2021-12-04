import DayThree_BinaryDiagnostic.PartOne.getPowerConsumption
import DayThree_BinaryDiagnostic.PartTwo.{getAirRating, getLifeSupportRating}
import Utils.readFileAsBinaryNumbers

import scala.collection.mutable

object DayThree_BinaryDiagnostic {
  case class BinaryCountTuple(var zeroes: Int, var ones: Int)

  def getRateMap(binaryNums: List[Int], lastDigit: Int): mutable.HashMap[Int, BinaryCountTuple] = {
    val rateMap = mutable.HashMap[Int, BinaryCountTuple]()

    binaryNums.foreach(binaryNum => {
      (0 to lastDigit).foreach(digit => {
        val shiftNum = Math.abs(digit - lastDigit)
        val digitValue = (binaryNum >> shiftNum) & 1
        var newZeroes = 0
        var newOnes = 0

        if (rateMap.contains(digit)) {
          rateMap.get(digit) match {
            case Some(BinaryCountTuple(zeroes, ones)) =>
              newZeroes = newZeroes + zeroes
              newOnes = newOnes + ones
          }
        }

        if (digitValue == 0) {
          newZeroes = newZeroes + 1
        } else {
          newOnes = newOnes + 1
        }

        rateMap.put(digit, BinaryCountTuple(newZeroes, newOnes))
      })
    })

    rateMap
  }

  object PartOne {
    def getPowerConsumption(binaryNums: List[Int], numDigits: Int): Int = {
      val gammaBits = Array.fill(numDigits)(0)
      val epsilonBits = Array.fill(numDigits)(0)
      val lastDigit = numDigits - 1
      val rateMap = getRateMap(binaryNums, lastDigit)

      (0 to lastDigit).foreach(digit => {
        rateMap.get(digit) match {
          case Some(BinaryCountTuple(zeroes, ones)) =>
            if (zeroes > ones) {
              gammaBits(digit)  = 0
              epsilonBits(digit) = 1
            } else {
              gammaBits(digit) = 1
              epsilonBits(digit) = 0
            }
        }
      })

      val gammaRate = (0 to lastDigit).toList.foldLeft(0)((acc, curr) => {
        val exponent = Math.abs(curr - lastDigit)
        acc + gammaBits(curr) * Math.pow(2, exponent).toInt
      })
      val epsilonRate = (0 to lastDigit).toList.foldLeft(0)((acc, curr) => {
        val exponent = Math.abs(curr - lastDigit)
        acc + epsilonBits(curr) * Math.pow(2, exponent).toInt
      })

      gammaRate * epsilonRate
    }
  }

  object PartTwo {
    def getDigitValue(binaryNum: Int, digit: Int, lastDigit: Int): Int = {
      (binaryNum >> Math.abs(digit - lastDigit)) & 1
    }

    def findZeroes(origSet: mutable.Set[Int], digit: Int, lastDigit: Int): mutable.Set[Int] = {
      val set = mutable.Set[Int]()
      origSet.foreach(num => {
        if (getDigitValue(num, digit, lastDigit) == 0) {
          set += num
        }
      })
      set
    }

    def findOnes(origSet: mutable.Set[Int], digit: Int, lastDigit: Int): mutable.Set[Int] = {
      val set = mutable.Set[Int]()
      origSet.foreach(num => {
        if (getDigitValue(num, digit, lastDigit) == 1) {
          set += num
        }
      })
      set
    }

    def getAirRating(binaryNums: List[Int], numDigits: Int, isOxygen: Boolean): Int = {
      val lastDigit = numDigits - 1
      val airSet = binaryNums.to(mutable.Set)
      var currDigit = 0

      while (airSet.size > 1) {
        val tuple = airSet.foldLeft(BinaryCountTuple(0, 0))((acc, currNum) => {
          acc match {
            case BinaryCountTuple(zeroes, ones) =>
              val digitValue = getDigitValue(currNum, currDigit, lastDigit)
              var newZeroes = zeroes
              var newOnes = ones

              if (digitValue == 0) {
                newZeroes = newZeroes + 1
              } else {
                newOnes = newOnes + 1
              }

              BinaryCountTuple(newZeroes, newOnes)
          }
        })

        tuple match {
          case BinaryCountTuple(zeroes, ones) =>
            if (isOxygen) {
              if (zeroes > ones) {
                airSet --= findOnes(airSet, currDigit, lastDigit)
              } else {
                airSet --= findZeroes(airSet, currDigit, lastDigit)
              }
            } else {
              if (zeroes > ones) {
                airSet --= findZeroes(airSet, currDigit, lastDigit)
              } else {
                airSet --= findOnes(airSet, currDigit, lastDigit)
              }
            }
        }

        currDigit = currDigit + 1
      }
      airSet.head
    }

    def getLifeSupportRating(binaryNums: List[Int], numDigits: Int): Int = {
      val oxygenRating = getAirRating(binaryNums, numDigits, isOxygen = true)
      val carbonDioxideRating = getAirRating(binaryNums, numDigits, isOxygen = false)
      oxygenRating * carbonDioxideRating
    }
  }

  def main(args: Array[String]): Unit = {
    val exampleInput = List("00100",
      "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000",
      "11001", "00010",
      "01010")
    val exampleInputAsInts = exampleInput.map(x => Integer.parseInt(x, 2))

    /* Part One */
    println(getPowerConsumption(exampleInputAsInts, 5))
    println(getPowerConsumption(readFileAsBinaryNumbers("day3input.txt"), 12))

    /* Part Two */
    println(getLifeSupportRating(exampleInputAsInts, 5))
    println(getLifeSupportRating(readFileAsBinaryNumbers("day3input.txt"), 12))
  }
}
