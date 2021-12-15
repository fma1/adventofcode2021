import DayFourteen_ExtendedPolymerization.PartOne.getMostAndLeastCommonElements
import DayFourteen_ExtendedPolymerization.PartTwo.executeSimulationAndGetMinMaxDiff

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math.BigDecimal.RoundingMode

object DayFourteen_ExtendedPolymerization {
  type Rules = Map[String, Char]
  type NewRules = Map[String, (String, String)]
  type PolymerMap = mutable.HashMap[String, BigInt]
  type PolymerData = (String, Rules)

  val BIG_INT_ONE = BigInt(1)

  def parseData(filename: String): PolymerData = {
    val bufferedSource = io.Source.fromInputStream(ClassLoader.getSystemResourceAsStream(filename))
    val iterator = bufferedSource.getLines().filter(_.nonEmpty)
    val polymerTemplate = iterator.next
    val rules =
      iterator.foldLeft(Map[String, Char]())((rules, line) => {
        val rulePair = line.trim.split("->")
        rules + (rulePair(0).trim -> rulePair(1).trim.apply(0))
      })
    bufferedSource.close()
    (polymerTemplate, rules)
  }

  object PartOne {
    @tailrec
    def constructNewPolymerTemplate(acc: List[Char], origLst: List[Char], newLst: List[Char]): List[Char] = {
      origLst match {
        case hd1::tl1 =>
          newLst match {
            case hd2::tl2 =>
              constructNewPolymerTemplate(hd2 +: hd1 +: acc, tl1, tl2)
            case Nil =>
              constructNewPolymerTemplate(hd1 +: acc, tl1, Nil)
          }
        case Nil =>
          acc
      }
    }

    def processPolymerTemplate(polymerTemplate: String, rules: Rules): String = {
      var idx = 0
      val buffer = new StringBuilder()
      val newElements = ArrayBuffer[Char]()

      while (idx <= polymerTemplate.length - 2) {
        buffer.clear()
        buffer.addOne(polymerTemplate(idx))
        buffer.addOne(polymerTemplate(idx + 1))
        newElements.append(rules(buffer.toString()))
        idx = idx + 1
      }

      constructNewPolymerTemplate(List(), polymerTemplate.toList, newElements.toList).reverse
        .mkString("")
    }

    def getMostAndLeastCommonElements(data: PolymerData, steps: Int): (BigInt, BigInt) = {
      data match {
        case (polymerTemplate, rules) =>
          val elemMap = mutable.HashMap[Char, BigInt]()
          polymerTemplate.foreach(ch => elemMap.put(ch, 0))
          rules.values.foreach(ch => elemMap.put(ch, 0))

          var tempPolymerTemplate = polymerTemplate
          (1 to steps).foreach(n => {
            tempPolymerTemplate = processPolymerTemplate(tempPolymerTemplate, rules)
            // println(s"After step $n: ${tempPolymerTemplate}")
          })

          tempPolymerTemplate.foreach(elem => {
            elemMap.put(elem, elemMap(elem) + 1)
          })

          // println(elemMap.mkString(" , "))
          (elemMap.values.min, elemMap.values.max)
      }
    }
  }

  object PartTwo {
    def step(polymerMap: PolymerMap, rules: Rules): PolymerMap = {
      val newPolymerMap = mutable.HashMap[String, BigInt]()

      polymerMap.foreach {
        case (elemPair, count) =>
          if (!rules.contains(elemPair)) {
            newPolymerMap.put(elemPair, count)
          } else {
            val newElem = rules(elemPair).toString
            val newElem1 = elemPair(0).toString + newElem
            val newElem2 = newElem + elemPair(1).toString

            val remappingFunc: Option[BigInt] => Option[BigInt] = {
              case Some(value) => Some(count + value)
              case None => Some(count)
            }

            newPolymerMap.updateWith(newElem1)(remappingFunc)
            newPolymerMap.updateWith(newElem2)(remappingFunc)
          }
      }

      newPolymerMap
    }

    def polymerStrToMap(polymerTemplate: String): PolymerMap = {
      val polymerMap = mutable.HashMap[String, BigInt]()
      val initialChar = polymerTemplate(0)
      polymerTemplate.tail.foldLeft(initialChar)((prev, curr) => {
        val elemPair = prev.toString + curr.toString
        polymerMap.updateWith(elemPair)({
          case Some(value) => Some(BIG_INT_ONE + value)
          case None => Some(BIG_INT_ONE)
        })
        curr
      })
      polymerMap
    }

    def executeSimulationAndGetMinMaxDiff(data: PolymerData, steps: Int): BigInt = {
      data match {
        case (polymerTemplate, rules) =>
          var polymerMap = polymerStrToMap(polymerTemplate)

          (1 to steps).foreach(_ => {
            polymerMap = step(polymerMap, rules)
          })

          // println(polymerMap.mkString(","))

          val singleElementMap =
            polymerMap.foldLeft(Map[Char, BigInt]())((acc, curr) => {
              curr match {
                case (elemPair, count) =>
                  val remappingFunc: Option[BigInt] => Option[BigInt] = {
                    case Some(value) => Some(count + value)
                    case None => Some(count)
                  }
                  acc.updatedWith(elemPair(0))(remappingFunc)
                    .updatedWith(elemPair(1))(remappingFunc)
              }
            }).map {
              case (elemPair, count) =>
                val countAsDec = BigDecimal(count)
                val result = countAsDec / BigDecimal(2.0)
                (elemPair, result.setScale(0, RoundingMode.CEILING).toBigInt)
            }

          // println(singleElementMap.mkString(","))
          singleElementMap.values.max - singleElementMap.values.min
      }
    }
  }


  def main(args: Array[String]): Unit = {
    /* Part One */
    var tuple = (BigInt(0), BigInt(0))
    tuple = getMostAndLeastCommonElements(parseData("day14input_example.txt"), 10)
    println(tuple._2 - tuple._1)
    tuple = getMostAndLeastCommonElements(parseData("day14input.txt"), 10)
    println(tuple._2 - tuple._1)

    /* Part Two */
    println(executeSimulationAndGetMinMaxDiff(parseData("day14input_example.txt"), 40))
    println(executeSimulationAndGetMinMaxDiff(parseData("day14input.txt"), 40))
  }
}
