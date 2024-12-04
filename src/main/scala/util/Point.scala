package util

case class Point(x: Int, y: Int) {
  override def toString: String = s"($x,$y)"

  def dist(p: Point) = Math.abs(x - p.x) + Math.abs(y - p.y)
}
