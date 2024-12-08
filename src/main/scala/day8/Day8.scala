package day8
import util.{Board, Point, ReadFile}

object Day8 extends ReadFile {
  val input: String                = readFile("src/main/scala/day8/input")
  val example: String              =
    """............
      |........0...
      |.....0......
      |.......0....
      |....0.......
      |......A.....
      |............
      |............
      |........A...
      |.........A..
      |............
      |............""".stripMargin

  def main(args: Array[String]): Unit = {
    println(solve1(example))
    println(solve1(input))
    println(solve2(example))
    println(solve2(input))
  }
  def read(s: String): Board[Char] = Board(s)
  private def solve1(s: String): Int = {
    val b = read(s)
    println(b.noSpace)
    groups(b)
      .foldLeft[Seq[Point]](Seq()) {
        case (acc, mp) =>
          val points = mp._2
          acc ++ Point.permutations(points).flatMap(pair => antinodes(pair.l, pair.r, b))
      }
      .distinct
      .size
  }
  private def solve2(s: String): Int = {
    val b = read(s)
    println(b.noSpace)
    groups(b)
      .foldLeft[Seq[Point]](Seq()) {
        case (acc, mp) =>
          val points = mp._2
          acc ++ Point.permutations(points).flatMap(pair => antinodes2(pair.l, pair.r, b))
      }
      .distinct
      .size
  }
  def antinodes(p1: Point, p2: Point, b: Board[Char]): Seq[Point] = {
    if (p1 == p2) Seq()
    else {
      val dx    = Math.abs(p1.x - p2.x)
      val dy    = Math.abs(p1.y - p2.y)
      val left  = Point.left(p1, p2)
      val right = Point.right(p1, p2)
      val all   = if (left == Point.top(p1, p2)) {
        Seq(Point(left.x - dx, left.y - dy), Point(right.x + dx, right.y + dy))
      } else {
        Seq(Point(left.x - dx, left.y + dy), Point(right.x + dx, right.y - dy))
      }
      all.filter(b.inside)
    }
  }
  def antinodes2(p1: Point, p2: Point, b: Board[Char]): Seq[Point] = {
    if (p1 == p2) Seq()
    else {
      val dx    = Math.abs(p1.x - p2.x)
      val dy    = Math.abs(p1.y - p2.y)
      val left  = Point.left(p1, p2)
      val right = Point.right(p1, p2)
      val all   = if (left == Point.top(p1, p2)) {
        var lefters  = Seq[Point]()
        var walker   = Point(left.x, left.y)
        while (b.inside(walker)) {
          lefters = lefters :+ walker
          walker = Point(walker.x - dx, walker.y - dy)
        }
        var righters = Seq[Point]()
        walker = Point(right.x, right.y)
        while (b.inside(walker)) {
          righters = righters :+ walker
          walker = Point(walker.x + dx, walker.y + dy)
        }
        (lefters ++ righters ++
          Seq(Point(left.x - dx, left.y - dy), Point(right.x + dx, right.y + dy))).distinct
      } else {
        var lefters  = Seq[Point]()
        var walker   = Point(left.x, left.y)
        while (b.inside(walker)) {
          lefters = lefters :+ walker
          walker = Point(walker.x - dx, walker.y + dy)
        }
        var righters = Seq[Point]()
        walker = Point(right.x, right.y)
        while (b.inside(walker)) {
          righters = righters :+ walker
          walker = Point(walker.x + dx, walker.y - dy)
        }
        (lefters ++ righters ++
          Seq(Point(left.x - dx, left.y + dy), Point(right.x + dx, right.y - dy))).distinct
      }
      all.filter(b.inside)
    }
  }
  def groups(b: Board[Char]) = {
    b.all.foldLeft[Map[Char, Seq[Point]]](Map()) {
      case (accmap, point) =>
        val c = b.value(point)
        if (c == '.') accmap
        else {
          if (accmap.contains(c)) accmap.updated(c, accmap(c) :+ point)
          else accmap.updated(c, Seq(point))
        }
    }
  }
}
