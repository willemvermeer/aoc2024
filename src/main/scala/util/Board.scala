package util

case class Board[T](rows: Seq[Seq[T]]) {
  def value(x: Int, y: Int): T = rows(y)(x)
  def value(p: Point): T       = value(p.x, p.y)
  def width                    = rows.head.length
  def height                   = rows.length

  def update(elt: T, pos: Point) = {
    Board(rows.zipWithIndex.map {
      case (row, r) =>
        row.zipWithIndex.map {
          case (col, c) =>
            if (Point(c, r) == pos) elt else col
        }
    })
  }
  def position(elt: T): Point = {
    (for {
      row <- (0 until height)
      col <- (0 until width)
      if value(col, row) == elt
    } yield {
      Point(col, row)
    }).head
  }

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

  def adjHV(p: Point) =
    Seq(
      Point(p.x, p.y - 1),
      Point(p.x - 1, p.y),
      Point(p.x + 1, p.y),
      Point(p.x, p.y + 1)
    ).filter(p => p.x >= 0 && p.y >= 0 && p.x < width && p.y < height)

  override def toString: String =
    rows.map(row => row.mkString(" ")).mkString("\n")

  def outside(p: Point): Boolean =
    p.x < 0 || p.y < 0 || p.x >= width || p.y >= height

  def inside(p: Point): Boolean = !outside(p)

  def noSpace: String = {
    rows.map(row => row.mkString("")).mkString("\n")
  }

  def swap(p1: Point, p2: Point): Board[T] = {
    val v1 = value(p1)
    val v2 = value(p2)
    update(v1, p2).update(v2, p1)
  }

  def all: Seq[Point] =
    for {
      row <- (0 until height)
      col <- (0 until width)
    } yield {
      Point(col, row)
    }
}

object Board {
  def apply(s: String): Board[Char] =
    Board[Char](s.split("\n").map(_.toList.toSeq).toSeq)
}
