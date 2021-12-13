import DayTwelve_PassagePathing.PartOne.backtrack
import DayTwelve_PassagePathing.PartTwo.backtrackV2

import scala.collection.concurrent.TrieMap
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

object DayTwelve_PassagePathing {
  type AdjacencyList = Map[Node, List[Node]]

  sealed trait Node
  case class BigCave(id: String) extends Node {
    override def toString: String = id
  }
  case class SmallCave(id: String) extends Node {
    override def toString: String = id
  }
  case object Start extends Node
  case object End extends Node

  def toCave(rawId: String): Node = {
    rawId match {
      case "start" => Start
      case "end" => End
      case _ =>
        if (rawId.charAt(0).isUpper) {
          BigCave(rawId)
        } else {
          SmallCave(rawId)
        }
    }
  }

  def parseData(filename: String): AdjacencyList = {
    val bufferedSource = io.Source.fromInputStream(ClassLoader.getSystemResourceAsStream(filename))
    val arrayOfCavePairs = bufferedSource.getLines().toArray
      .map(_.trim.split("-")
        .map(x => toCave(x)))
    bufferedSource.close()

    val adjacencyList =
      arrayOfCavePairs.foldLeft(Map[Node, List[Node]]())((map, curr) => {
        // v1 -> v2
        val v1 = curr(0)
        val v2 = curr(1)
        map.updatedWith(v1)({
          case Some(lst) => Some(v2 +: lst)
          case None => Some(List(v2))
        }).updatedWith(v2)({
          case Some(lst) => Some(v1 +: lst)
          case None => Some(List(v1))
        })
      })

    adjacencyList
  }

  object PartOne {
    def backtrack(currNode: Node, path: List[Node], graph: AdjacencyList, results: ArrayBuffer[List[Node]], seen: Set[Node]): Unit = {
      // println(s"currNode: $currNode")
      currNode match {
        case End =>
          results += path
        case _ =>
          graph(currNode).foreach {
            case Start =>
              /*
               * Without this check,
               * StackOverflowError ensues
               */
              ()
            case nextNode =>
              if (!seen(nextNode)) {
                currNode match {
                  case SmallCave(_) =>
                    backtrack(nextNode, nextNode +: path, graph, results, seen + currNode)
                  case _ =>
                    backtrack(nextNode, nextNode +: path, graph, results, seen)
                }
              }
          }
      }
    }
  }

  object PartTwo {
    def backtrackV2(currNode: Node, path: List[Node], graph: AdjacencyList, results: ArrayBuffer[List[Node]], max: Int, seen: TrieMap[Node, Int]): Unit = {
      // println(s"currNode: $currNode")
      currNode match {
        case End =>
          results += path
        case _ =>
          graph(currNode).foreach {
            case Start =>
              ()
            case nextNode =>
              if (!seen.contains(nextNode) || seen(nextNode) < 2) {
                currNode match {
                  case SmallCave(_) =>
                    if (max == 2 && seen.getOrElse(currNode, 0) == 1) {
                      ()
                    } else {
                      backtrackV2(nextNode, nextNode +: path, graph, results, Math.max(max, (seen.getOrElse(currNode, 0) + 1)),
                        seen + (currNode -> (seen.getOrElse(currNode, 0) + 1)))
                    }
                  case _ =>
                    backtrackV2(nextNode, nextNode +: path, graph, results, max, seen)
                }
              }
          }
      }
    }
  }


  def main(args: Array[String]): Unit = {
    var graph: AdjacencyList = Map()
    val results = ArrayBuffer[List[Node]]()

    /* Part One */
    graph = parseData("day12input_example1.txt")
    // graph = parseData("day12input_example2.txt")
    // graph = parseData("day12input_example3.txt")
    backtrack(Start, List[Node](), graph, results, Set[Node]())
    println(results.size)

    results.clear()
    graph = parseData("day12input.txt")
    backtrack(Start, List[Node](), graph, results, Set[Node]())
    println(results.size)

    /* Part Two */
    results.clear()
    graph = parseData("day12input_example1.txt")
    // graph = parseData("day12input_example2.txt")
    backtrackV2(Start, List[Node](), graph, results, 0, TrieMap[Node, Int]())
    println(results.size)

    results.clear()
    graph = parseData("day12input.txt")
    backtrackV2(Start, List[Node](), graph, results, 0, TrieMap[Node, Int]())
    println(results.size)
  }
}
