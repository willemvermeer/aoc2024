package day9
import util.ReadFile

object Day9 extends ReadFile {
  val input: String   = readFile("src/main/scala/day9/input")
  val example: String = "2333133121414131402"

  def main(args: Array[String]): Unit = {
//    println(solve1(example))
//    println(solve1(input))
    println(solve2(example))
    println(solve2(input))
  }
  private def solve1(s: String) = {
    var res = layout(s.trim())
    while (res.indexOf(".") >= 0) {
      val right = res.takeRight(1).head
      val pos   = res.indexOf(".")
      res = res.dropRight(1).updated(pos, right)
    }
    res.zipWithIndex.map { case (d, i) => (d.trim().toInt) * i }.map(x => BigInt(x)).sum
  }
  private def layout(s: String) = {
    var index  = 0
    var sIndex = 0
    var result = Seq[String]()
    while (sIndex < s.length) {
      val c = s.charAt(sIndex).toString.toInt
      if (sIndex % 2 == 0) {
        result = result ++ ((0 until c).map(_ => index.toString))
        index = index + 1
      } else {
        result = result ++ ((0 until c).map(_ => "."))
      }
      sIndex = sIndex + 1
    }
    result
  }
  private def solve2(s: String): BigInt = {
    var res = layout(s.trim())
    var max = res.takeRight(1).head.toInt
    while (max > 0) {
      val start = res.indexOf(max.toString)
      val num   = res.count(_ == max.toString)
      val f     = fits(num, res)
      if (f > 0 && f <= start) {
        (0 until num).foreach { i => res = res.updated(f + i, max.toString) }
        (0 until num).foreach { i => res = res.updated(start + i, ".") }
      }
      max = max - 1
    }
    res.zipWithIndex
      .map {
        case (d, i) =>
          val v = d.trim()
          if (v != ".")
            (d.trim().toInt) * i
          else
            0
      }
      .map(x => BigInt(x))
      .sum
  }

  private def fits(len: Int, res: Seq[String]) = {
    val z = (0 until len).map(_ => ".").mkString("")
    res.map(s => if (s == ".") "." else ",").mkString("").indexOf(z)
  }
}
