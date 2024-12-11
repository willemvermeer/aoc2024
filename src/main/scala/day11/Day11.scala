package day11
import util.ReadFile

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.{ArrayIsParallelizable, ImmutableIterableIsParallelizable}

object Day11 extends ReadFile {
  val input: String   = readFile("src/main/scala/day11/input")
  val example: String = "125 17"

  def main(args: Array[String]): Unit = {
    println(solve1(example)) // 55312 bij 25 levels
    println(solve1(input))
    println(solve2(example))
    println(solve2(input))
  }
  private def solve1(s: String) = {
    val stones = s.trim().split(" ").map(BigInt(_))
    val result = (0 until 25).foldLeft[Seq[BigInt]](stones) { (acc, i) =>
      blink(acc)
    }
    result.size
  }
  def blink(stones: Seq[BigInt]): Seq[BigInt] = {
    stones.flatMap { stone =>
      if (stone == 0) Seq(1)
      else {
        val strStone = stone.toString
        if (strStone.length % 2 == 0) {
          Seq(BigInt(strStone.substring(0, strStone.length / 2)), BigInt(strStone.substring(strStone.length / 2).toInt))
        } else {
          Seq(stone * 2024)
        }
      }
    }
  }
  private def solve2(s: String) = {
    def next(stone: BigInt, level: Int, maxLevel: Int, cacheValues: mutable.Map[CacheKey, BigInt]) = {
      if (stone == 0) blink(1, level + 1, maxLevel, cacheValues)
      else {
        val strStone = stone.toString
        if (strStone.length % 2 == 0) {
          blink(BigInt(strStone.substring(0, strStone.length / 2)).toInt, level + 1, maxLevel, cacheValues) +
            blink(BigInt(strStone.substring(strStone.length / 2)).toInt, level + 1, maxLevel, cacheValues)
        } else {
          blink(stone * 2024, level + 1, maxLevel, cacheValues)
        }
      }
    }

    def blink(stone: BigInt, level: Int, maxLevel: Int, cacheValues: mutable.Map[CacheKey, BigInt]): BigInt = {
      if (level == maxLevel) 1
      else {
        if (stone < 10) {
          val cacheKey = CacheKey(stone, level)
          if (cacheValues.contains(cacheKey)) {
            cacheValues(cacheKey)
          } else {
            val result = next(stone, level, maxLevel, cacheValues)
            cacheValues.put(cacheKey, result)
            result
          }
        } else
          next(stone, level, maxLevel, cacheValues)
      }
    }
    val cacheValues: mutable.Map[CacheKey, BigInt] = mutable.Map()
    val stones                                     = s.trim().split(" ").map(BigInt(_))
    stones.toSeq.par.map(st => blink(st, 0, 75, cacheValues)).sum
  }
}
case class CacheKey(digit: BigInt, level: Int)
