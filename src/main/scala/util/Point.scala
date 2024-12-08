package util

case class Point(x: Int, y: Int) {
  override def toString: String = s"($x,$y)"

  def dist(p: Point) = Math.abs(x - p.x) + Math.abs(y - p.y)
}

object Point {
  def left(p1: Point, p2: Point): Point  =
    if (p1.x < p2.x) p1 else p2
  def right(p1: Point, p2: Point): Point =
    if (p1.x > p2.x) p1 else p2
  def top(p1: Point, p2: Point): Point   =
    if (p1.y < p2.y) p1 else p2

  def permutations(ps: Seq[Point]) = {
    val all = for {
      p1 <- ps
      p2 <- ps
      if (p1 != p2)
    } yield (Pair[Point](p1, p2))
    all.foldLeft[Seq[Pair[Point]]](Seq()) {
      case (acc, elt) =>
        if (acc.contains(elt) || acc.contains(elt.flip)) acc
        else acc :+ Pair(elt.l, elt.r)
    }
  }

}
