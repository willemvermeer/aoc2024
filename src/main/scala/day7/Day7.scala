package day7
import util.ReadFile

case class Equation(target: BigInt, nums: Seq[Int]) {
  override def toString: String = s"$target: ${nums.mkString(" ")}"
}
case class Result(r: BigInt, e: Equation)

object Day7 extends ReadFile {
  val input1: String             = readFile("src/main/scala/day7/input1")
  val example                    =
    """190: 10 19
      |3267: 81 40 27
      |83: 17 5
      |156: 15 6
      |7290: 6 8 6 15
      |161011: 16 10 13
      |192: 17 8 14
      |21037: 9 7 18 13
      |292: 11 6 16 20""".stripMargin

  def main(args: Array[String]): Unit = {
    println(solve1(example))
    println(solve1(input1))
    println(solve2(example))
    println(solve2(input1))
  }
  def read(s: String) = {
    s.split("\n")
      .map { line =>
        val splits = line.split(":")
        Equation(BigInt(splits(0)), splits(1).trim().split(" ").map(_.toInt))
      }
      .toSeq
  }
  private def solve1(s: String) = {
    val equations = read(s)
    equations.map(e => oneResult(all(e))).sum
  }
  private def oneResult(permutations: Seq[Result]): BigInt = {
    val filtered = permutations.filter(res => res.r == res.e.target)
    if (filtered.isEmpty) 0
    else filtered.head.r
  }
  private def all(eq: Equation)  =
    eq.nums.tail.foldLeft[Seq[Result]](Seq(Result(eq.nums.head, eq))) {
      case (acc, elt) =>
        acc.flatMap { r =>
          val r1 = Result(r.r * elt, r.e)
          val r2 = Result(r.r + elt, r.e)
          Seq(r1, r2)
        }
    }
  private def solve2(s: String) = {
    val equations = read(s)
    equations.map(e => oneResult(all2(e))).sum
  }
  private def all2(eq: Equation) =
    eq.nums.tail.foldLeft[Seq[Result]](Seq(Result(eq.nums.head, eq))) {
      case (acc, elt) =>
        acc.flatMap { r =>
          val r1 = Result(r.r * elt, r.e)
          val r2 = Result(r.r + elt, r.e)
          val r3 = Result(BigInt(s"${r.r}$elt"), r.e)
          Seq(r1, r2, r3)
        }
    }
}
