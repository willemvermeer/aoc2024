package util

case class Path(entries: Seq[Point]) {
  def add(p: Point): Path = Path(this.entries :+ p)

  override def toString: String = entries.mkString("-")
}
