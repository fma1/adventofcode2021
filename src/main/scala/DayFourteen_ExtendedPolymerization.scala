import DayFourteen_ExtendedPolymerization.PartOne.getMostAndLeastCommonElements
import DayFourteen_ExtendedPolymerization.PartTwo.getMostAndLeastCommonElementsV2

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object DayFourteen_ExtendedPolymerization {
  type Rules = Map[String, Char]
  type NewRules = Map[String, (String, String)]
  type PolymerMap = mutable.HashMap[String, Int]
  type PolymerData = (String, Rules)

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

    def getMostAndLeastCommonElements(data: PolymerData, steps: Int): (Int, Int) = {
      data match {
        case (polymerTemplate, rules) =>
          val elemMap = mutable.HashMap[Char, Int]()
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
    def initializePolymerMap(polymerTemplate: String, polymerMap: PolymerMap): Unit = {
      var idx = 0
      val buffer = new StringBuilder()

      while (idx <= polymerTemplate.length - 2) {
        buffer.clear()
        buffer.addOne(polymerTemplate(idx))
        buffer.addOne(polymerTemplate(idx + 1))
        polymerMap.updateWith(buffer.toString())({
          case Some(value) => Some(1 + value)
          case None => Some(1)
        })
        idx = idx + 1
      }
    }

    def processPolymerTemplateV2(polymerMap: PolymerMap, rules: NewRules): Unit = {
      val origMap = polymerMap.toList.toMap

      polymerMap.clear()
      origMap.foreach {
        case (elem, count) =>
          polymerMap.put(elem, 0)

          val remappingFunc: Option[Int] => Option[Int] = {
            case Some(value) => Some(count + value)
            case None => Some(count)
          }

          val newElems = rules(elem)
          polymerMap.updateWith(newElems._1)(remappingFunc)
          polymerMap.updateWith(newElems._2)(remappingFunc)
      }
    }

    def getMostAndLeastCommonElementsV2(data: PolymerData, steps: Int): (Int, Int) = {
      data match {
        case (polymerTemplate, rules) =>
          val singleElemMap = mutable.HashMap[Char, Int]()
          polymerTemplate.foreach(ch => singleElemMap.put(ch, 0))
          rules.values.foreach(ch => singleElemMap.put(ch, 0))

          val newRules =
            rules.map {
              case (pair, elem) =>
                (pair, (pair(0).toString + elem.toString, elem.toString + pair(1).toString))
            }

          val polymerMap = mutable.HashMap[String, Int]()
          initializePolymerMap(polymerTemplate, polymerMap)
          (1 to steps).foreach(n => {
            processPolymerTemplateV2(polymerMap, newRules)
            // tempPolymerTemplate = processPolymerTemplate(tempPolymerTemplate, rules)
            // println(s"After step $n: ${tempPolymerTemplate}")
          })

          polymerMap.foreach {
            case (pair, count) =>
              val elem1 = pair(0)
              val elem2 = pair(1)

              singleElemMap.updateWith(elem1)({
                case Some(value) => Some(count + value)
                case None => Some(count)
              })
              singleElemMap.updateWith(elem2)({
                case Some(value) => Some(count + value)
                case None => Some(count)
              })
          }

          // println(elemMap.mkString(" , "))
          (singleElemMap.values.min, singleElemMap.values.max)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    var tuple = (0, 0)
    /*
    tuple = getMostAndLeastCommonElements(parseData("day14input_example.txt"), 10)
    println(tuple._2 - tuple._1)
    tuple = getMostAndLeastCommonElements(parseData("day14input.txt"), 10)
    println(tuple._2 - tuple._1)
     */

    // TODO: Fix Part 2
    tuple = getMostAndLeastCommonElementsV2(parseData("day14input_example.txt"), 40)
    println(tuple._2 - tuple._1)
  }
}
