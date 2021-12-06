import DaySix_Lanternfish.PartOne.{getInitialFishTimers, simulateLanternfishAndGetPop}
import DaySix_Lanternfish.PartTwo.simulateLanternfishAndGetPopWithHistogram

import scala.collection.mutable

object DaySix_Lanternfish {
  object PartOne {
    def getInitialFishTimers(filename: String): Array[Long] = {
      val bufferedSource = io.Source.fromInputStream(ClassLoader.getSystemResourceAsStream(filename))
      val numbersStr = bufferedSource.getLines().next()
      bufferedSource.close
      numbersStr.split(",").map(_.trim.toLong)
    }
    def simulateLanternfishAndGetPop(fishTimers: Array[Long], days: Int): Long = {
      (0 until days).foldLeft(fishTimers.toList)((acc, _) => {
        acc.foldLeft(List[Long]())((acc, curr) => {
          curr match {
            case 0 =>
              6L +: 8L +: acc
            case n =>
              (n - 1) +: acc
          }
        })
      }).size
    }
  }

  object PartTwo {
    def simulateLanternfishAndGetPopWithHistogram(fishTimers: Array[Long], days: Int): Long = {
      val initialMap = Map(0L -> 0L, 1L -> 0L, 2L -> 0L, 3L -> 0L, 4L -> 0L, 5L -> 0L, 6L -> 0L, 7L -> 0L, 8L -> 0L)
      var fishMap = mutable.HashMap[Long, Long]()
      fishMap ++= initialMap
      fishTimers.foreach(x => { fishMap(x) = fishMap(x) + 1 })
      (0 until days).foreach(_ => {
        val newFishMap = mutable.HashMap[Long, Long]()
        newFishMap ++= initialMap
        fishMap.foreach {
          case (id, _) =>
            id match {
              case 0 =>
                newFishMap(6) = fishMap(0) + newFishMap(6)
                newFishMap(8) = fishMap(0) + newFishMap(8)
              case n =>
                newFishMap(n - 1) = fishMap(n) + newFishMap(n - 1)
            }
        }
        fishMap = newFishMap
      })
      fishMap.values.sum
    }
  }

  def main(args: Array[String]): Unit = {
    /* Part One */
    println(simulateLanternfishAndGetPop(
      getInitialFishTimers("day6input_example.txt"), 80))
    // println(simulateLanternfishAndGetPop(getInitialFishTimers("day6input.txt"), 80))
    /* Part Two */
    /*
    println(simulateLanternfishAndGetPopWithHistogram(
      getInitialFishTimers("day6input_example.txt"), 256))
     */
    println(simulateLanternfishAndGetPopWithHistogram(
      getInitialFishTimers("day6input.txt"), 256))
  }
}
