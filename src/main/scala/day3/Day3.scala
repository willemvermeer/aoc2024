package day3
import util.ReadFile

import scala.util.matching.Regex
case class Mul(x: Int, y: Int) {
  def mul: Int = x * y
}

object Mul {
  def apply(s: String): Mul = {
    val s1 = s.substring("mul(".length)
    val s2 = s1.dropRight(1)
    val s3 = s2.split(",")
    Mul(s3(0).toInt, s3(1).toInt)
  }
}

object Day3 extends ReadFile {
  val input1: String = readFile("src/main/scala/day3/input1")
  val example        =
    """xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))""".stripMargin

  val mulPattern: Regex = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)".r

  def parseline(s: String): Seq[Mul] = {
    mulPattern.findAllMatchIn(s).toSeq.map(s => Mul(s.toString()))
  }

  def main(args: Array[String]): Unit = {
    println(solve1(example))
    println(solve1(input1))
    println(solve2(input1))
  }
  private def mulLine(seq: Seq[Mul]) = seq.foldLeft(0)((acc, m) => acc + m.mul)
  private def solve1(s: String) = {
    s.split("\n").map(l => mulLine(parseline(l))).sum
  }
  private def solve2(s: String) = {
    val start = "do()" + s
    start
      .split("don't()")
      .map { line =>
        if (line.contains("do()")) {
          val telmee = line.substring(line.indexOf("do()") + 4)
          println(line)
          println(telmee)
          mulLine(parseline(telmee))
        } else 0
      }
      .sum
  }
}
