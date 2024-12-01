package day1
import util.ReadFile

object Day1 extends ReadFile {
  val input1: String = readFile("src/main/scala/day1/input1")

  def main(args: Array[String]): Unit = {
    println(solve1)
    println(solve2)
  }
  val pairs: Seq[(Int, Int)] = input1
    .split("\n")
    .map { line =>
      val splits = line.split("   ")
      (Integer.parseInt(splits(0)), Integer.parseInt(splits(1)))
    }
    .toSeq
  private def solve1 = {
    val l = pairs.map(_._1).sorted
    val r = pairs.map(_._2).sorted
    (l zip r).map { case (l, r) => Math.abs(l - r) }.sum
  }
  private def solve2 = {
    val r = pairs.map(_._2)
    pairs.foldLeft(0) {
      case (acc, pair) =>
        acc + pair._1 * r.filter(_ == pair._1).size
    }
  }
}
