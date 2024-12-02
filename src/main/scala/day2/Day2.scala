package day2
import util.ReadFile

object Day2 extends ReadFile {
  val input1: String = readFile("src/main/scala/day2/input1")

  def main(args: Array[String]): Unit = {
    println(solve1)
    println(solve2)
  }

  val example =
    """7 6 4 2 1
      |1 2 7 8 9
      |9 7 6 2 1
      |1 3 2 4 5
      |8 6 4 4 1
      |1 3 6 7 9
      |""".stripMargin

  private val rows = input1
    .split("\n")
    .map(
      _.split(" ").toSeq
        .map(s => Integer.parseInt(s))
    )

  def isSafe(row: Seq[Int]): Boolean         =
    row.sliding(2).forall { pair =>
      val l = pair.head
      val r = pair.tail.head
      (l < r && Math.abs(l - r) <= 3)
    } ||
      row.sliding(2).forall { pair =>
        val l = pair.head
        val r = pair.tail.head
        (r < l && Math.abs(l - r) <= 3)
      }

  private def solve1 = {
    rows.count(r => isSafe(r))
  }
  def removeIndex(row: Seq[Int], index: Int) =
    row.zipWithIndex.filterNot { case (entry, ix) => index == ix }.map(_._1)

  def isSafe2(row: Seq[Int]) = {
    val withouts = for {
      index <- Range(0, row.size)
    } yield {
      removeIndex(row, index)
    }
    withouts.exists(r => isSafe(r))
  }
  private def solve2 = {
    rows.count(r => isSafe2(r))
  }
}
