package day12
import util.{Board, Point, ReadFile}

object Day12 extends ReadFile {
  val input: String    = readFile("src/main/scala/day12/input")
  val example: String  = """AAAA
                          |BBCD
                          |BBCC
                          |EEEC""".stripMargin
  val example1: String = """OOOOO
                           |OXOXO
                           |OOOOO
                           |OXOXO
                           |OOOOO""".stripMargin
  val example2: String = """RRRRIICCFF
                           |RRRRIICCCF
                           |VVRRRCCFFF
                           |VVRCCCJFFF
                           |VVVVCJJCFE
                           |VVIVCCJJEE
                           |VVIIICJJEE
                           |MIIIIIJJEE
                           |MIIISIJEEE
                           |MMMISSJEEE""".stripMargin
  def main(args: Array[String]): Unit = {
//    println(solve1(example))
//    println(solve1(example1))
//    println(solve1(example2))
    println(solve1(input))
//    println(solve2(example))
//    println(solve2(example1))
    println(solve2(input))
    // solve2(input)
  }
  private def solve1(s: String) = {
    val regions = calcRegions(s)
    regions.flatMap(_._2.map(_.price)).sum
  }
  private def solve2(s: String) = {
    val regions    = calcRegions(s)
    val b          = Board(s.trim())
    val allRegions = regions.flatMap(_._2).toSeq
    allRegions.map { region =>
      val edges = region.points.foldLeft[Set[Edge]](Set()) {
        case (acc, elt) =>
          val max_four = Seq(
            if (isLeftEdge(elt, b)) {
              val ts = tops(elt).takeWhile(p => b.value(p) == b.value(elt) && isLeftEdge(p, b))
              val bs = bottoms(elt, b).takeWhile(p => b.value(p) == b.value(elt) && isLeftEdge(p, b))
              Some(Edge((ts ++ Seq(elt) ++ bs).toSet, "l"))
            } else None,
            if (isRightEdge(elt, b)) {
              val ts = tops(elt).takeWhile(p => b.value(p) == b.value(elt) && isRightEdge(p, b))
              val bs = bottoms(elt, b).takeWhile(p => b.value(p) == b.value(elt) && isRightEdge(p, b))
              Some(Edge((ts ++ Seq(elt) ++ bs).toSet, "r"))
            } else None,
            if (isTopEdge(elt, b)) {
              val l = lefts(elt).takeWhile(p => b.value(p) == b.value(elt) && isTopEdge(p, b))
              val r = rights(elt, b).takeWhile(p => b.value(p) == b.value(elt) && isTopEdge(p, b))
              Some(Edge((l ++ Seq(elt) ++ r).toSet, "t"))
            } else None,
            if (isBottomEdge(elt, b)) {
              val ts = lefts(elt).takeWhile(p => b.value(p) == b.value(elt) && isBottomEdge(p, b))
              val bs = rights(elt, b).takeWhile(p => b.value(p) == b.value(elt) && isBottomEdge(p, b))
              Some(Edge((ts ++ Seq(elt) ++ bs).toSet, "b"))
            } else None
          ).flatten
          acc ++ max_four
      }
      edges.size * region.points.size
    }.sum
  }

  private def isLeftEdge(elt: Point, b: Board[Char])   = elt.x == 0 || b.value(Point(elt.x - 1, elt.y)) != b.value(elt)
  private def isRightEdge(elt: Point, b: Board[Char])  =
    elt.x == b.width - 1 || b.value(Point(elt.x + 1, elt.y)) != b.value(elt)
  private def isTopEdge(elt: Point, b: Board[Char])    = elt.y == 0 || b.value(Point(elt.x, elt.y - 1)) != b.value(elt)
  private def isBottomEdge(elt: Point, b: Board[Char]) =
    elt.y == b.height - 1 || b.value(Point(elt.x, elt.y + 1)) != b.value(elt)

  private def tops(elt: Point)                    = (0 until elt.y).map(y => Point(elt.x, y)).reverse
  private def bottoms(elt: Point, b: Board[Char]) = (elt.y + 1 until b.height).map(y => Point(elt.x, y))
  private def lefts(elt: Point)                   = (0 until elt.x).map(x => Point(x, elt.y)).reverse
  private def rights(elt: Point, b: Board[Char])  = (elt.x + 1 until b.height).map(x => Point(x, elt.y))

  private def calcRegions(s: String): Map[Char, Seq[Region]] = {
    val b = Board(s.trim())
    b.all.groupBy(p => b.value(p)).map {
      case (c, points) =>
        val regions_1 = {
          points.foldLeft[Seq[Region]](Seq()) {
            case (acc, p) =>
              acc.find(region => region.adj(p)) match {
                case Some(r) => Region(r.points :+ p) +: acc.filterNot(_ == r)
                case None    => Region(Seq(p)) +: acc
              }
          }
        }
        var reg_ult = regions_1
        (0 until 100).foreach { _ =>
          reg_ult = reg_ult.foldLeft[Seq[Region]](Seq()) {
            case (acc, r) =>
              acc.find(region => region.isNeighboring(r)) match {
                case Some(r2) => Region(r2.points ++ r.points) +: acc.filterNot(_ == r2)
                case None     => r +: acc
              }
          }
        }
        c -> reg_ult
    }
  }
}
case class Region(points: Seq[Point]) {
  def adj(p: Point): Boolean            = points.exists(point => point.areNeighbours(p))
  def perimeter: Int = {
    points.map(p => 4 - p.neighbours(points).size).sum
  }
  def area: Int                         = points.length
  def price: Int                        = area * perimeter
  def isNeighboring(r: Region): Boolean = points.exists(p => r.points.exists(p.areNeighbours))
}
case class Edge(points: Set[Point], typ: String)
