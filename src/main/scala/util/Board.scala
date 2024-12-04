package util

case class Board[T](rows: Seq[Seq[T]]) {
  def value(x: Int, y: Int): T = rows(y)(x)
  def value(p: Point): T       = value(p.x, p.y)
  def width                    = rows.head.length
  def height                   = rows.length

  def adj(p: Point) =
    Seq(
      Point(p.x - 1, p.y - 1),
      Point(p.x, p.y - 1),
      Point(p.x + 1, p.y - 1),
      Point(p.x - 1, p.y),
      Point(p.x + 1, p.y),
      Point(p.x - 1, p.y + 1),
      Point(p.x, p.y + 1),
      Point(p.x + 1, p.y + 1)
    ).filter(p => p.x >= 0 && p.y >= 0 && p.x < width && p.y < height)

  override def toString: String =
    rows.map(row => row.mkString(" ")).mkString("\n")
}

object Board {
  def apply(s: String): Board[Char] =
    Board[Char](s.split("\n").map(_.toList.toSeq).toSeq)
}
