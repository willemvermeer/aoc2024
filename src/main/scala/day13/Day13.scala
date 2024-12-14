package day13
import util.ReadFile

object Day13 extends ReadFile {
  val input: String   = readFile("src/main/scala/day13/input")
  val example: String = """Button A: X+94, Y+34
                          |Button B: X+22, Y+67
                          |Prize: X=8400, Y=5400
                          |
                          |Button A: X+26, Y+66
                          |Button B: X+67, Y+21
                          |Prize: X=12748, Y=12176
                          |
                          |Button A: X+17, Y+86
                          |Button B: X+84, Y+37
                          |Prize: X=7870, Y=6450
                          |
                          |Button A: X+69, Y+23
                          |Button B: X+27, Y+71
                          |Prize: X=18641, Y=10279""".stripMargin

  def main(args: Array[String]): Unit = {
    println(solve1(example))
    println(solve1(input))
    println(solve2(example))
    println(solve2(input))
  }
  private def solve1(s: String): Int = {
    val arcades = read(s)
    arcades.map { arc =>
      val results = for {
        x <- 0 until 100
        y <- 0 until 100
        if arc.a.x * x + arc.b.x * y == arc.x && arc.a.y * x + arc.b.y * y == arc.y
      } yield 3 * x + y
      if (results.isEmpty) 0 else results.min
    }.sum
  }
  private def read(s: String): Seq[Arcade] = {
    s.trim()
      .split("\n\n")
      .map {
        case s"Button A: X+${xa}, Y+${ya}\nButton B: X+${xb}, Y+${yb}\nPrize: X=${xp}, Y=${yp}" =>
          Arcade(Button(xa.toInt, ya.toInt), Button(xb.toInt, yb.toInt), xp.toInt, yp.toInt)
      }
      .toSeq
  }
  private def solve2(s: String): Long = {
    val arcades = read(s)
    arcades
      .map(_.add)
      .map { arc =>
        val (x, y) = arc.solution
        val xint   = Math.round(x)
        val yint   = Math.round(y)
        val res1   = xint * arc.a.x + yint * arc.b.x
        val res2   = xint * arc.a.y + yint * arc.b.y
        if (res1 == arc.x && res2 == arc.y) {
          3 * xint + yint
        } else 0
      }
      .sum
  }
}
case class Button(x: Int, y: Int)
case class Arcade(a: Button, b: Button, x: Long, y: Long) {
  def add = Arcade(a, b, x + 10000000000000L, y + 10000000000000L)
  def solution = {
    val fact = (1.0 * b.x) / b.y
    val xr   = (fact * y - x) / ((a.y * fact) - a.x)
    val yr   = (1.0 * x - (a.x * xr)) / b.x
    (xr, yr)
  }
}
