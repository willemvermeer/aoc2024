package day14
import util.{Point, ReadFile}

object Day14 extends ReadFile {
  val input: String   = readFile("src/main/scala/day14/input")
  val example: String = """p=0,4 v=3,-3
                          |p=6,3 v=-1,-3
                          |p=10,3 v=-1,2
                          |p=2,0 v=2,-1
                          |p=0,0 v=1,3
                          |p=3,0 v=-2,-2
                          |p=7,6 v=-1,-3
                          |p=3,0 v=-1,-2
                          |p=9,3 v=2,3
                          |p=7,3 v=-1,2
                          |p=2,4 v=2,-3
                          |p=9,5 v=-3,-3""".stripMargin

  def main(args: Array[String]): Unit = {
//    println(solve1(example, 11, 7))
//    println(solve1(input, 101, 103))
    // println(solve1(input))
//     println(solve2(example, 11, 7))
    println(solve2(input, 101, 103))
  }
  private def solve1(s: String, w: Int, h: Int): Int = {
    val locations = read(s)
    val newLocs   = locations.map {
      case loc =>
        (0 until 100).foldLeft[Location](loc) {
          case (acc, _) =>
            acc.next(w, h)
        }
    }
    val q1        = newLocs.filter(l => l.p.x < w / 2 && l.p.y < h / 2)
    val q2        = newLocs.filter(l => l.p.x > w / 2 && l.p.y < h / 2)
    val q3        = newLocs.filter(l => l.p.x < w / 2 && l.p.y > h / 2)
    val q4        = newLocs.filter(l => l.p.x > w / 2 && l.p.y > h / 2)
    q1.size * q2.size * q3.size * q4.size
  }
  def read(s: String): Seq[Location] =
    s.trim().split("\n").map {
      case s"p=${x},${y} v=${dx},${dy}" =>
        Location(Point(x.toInt, y.toInt), dx.toInt, dy.toInt)
    }

  private def solve2(s: String, w: Int, h: Int): Int = {
    var locations = read(s)
    var i         = 0
    while (!itsEaster(locations, w)) {
      i = i + 1
      locations = locations.map(_.next(w, h))
      val hits = numHits(locations, w)
      if (hits >= 529) {
        val points = locations.map(_.p)
        println(hits + ":" + i)
        pr(points, w, h)
      }
    }
    i
  }
  def pr(points: Seq[Point], w: Int, h: Int): Unit = {
    (0 until h).foreach { y =>
      (0 until w).foreach { x =>
        if (points.contains(Point(x, y))) print("*") else print(".")
      }
      println
    }
  }
  def itsEaster(locations: Seq[Location], w: Int) = {
    val allPoints = locations.map(_.p).distinct
    allPoints.forall { p =>
      (p.x == w / 2) || allPoints.contains(Point(w - p.x - 1, p.y))
    }
  }
  def numHits(locations: Seq[Location], w: Int): Int = {
    val allPoints = locations.map(_.p).distinct
    (for {
      p1 <- allPoints
      p2 <- allPoints
      if p1 != p2 && p1.areNeighbours(p2)
    } yield Set(p1, p2)).distinct.size
  }

}
case class Location(p: Point, dx: Int, dy: Int) {
  def next(w: Int, h: Int) =
    Location(Point((w + p.x + dx) % w, (h + p.y + dy) % h), dx, dy)
}
