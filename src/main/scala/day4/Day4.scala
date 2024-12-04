package day4
import util.{Board, Path, Point, ReadFile}

object Day4 extends ReadFile {
  val input1: String  = readFile("src/main/scala/day4/input1")
  val example: String =
    """MMMSXXMASM
      |MSAMXMSMSA
      |AMXSXMAAMM
      |MSAMASMSMX
      |XMASAMXAMM
      |XXAMMXXAMA
      |SMSMSASXSS
      |SAXAMASAAA
      |MAMMMXMMMM
      |MXMXAXMASX
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    println(Board(example))
    println(solve1(example))
    println(solve1(input1))
    println(solve2(example))
    println(solve2(input1))

  }
  private def solve1(input: String) = {
    val b = Board(input)
    next(b, next(b, next(b, getXs(b, 'X'), 'M'), 'A'), 'S').count(applies)
  }

  private def solve2(input: String) = {
    val b     = Board(input)
    val mases = next(b, next(b, getXs(b, 'M'), 'A'), 'S').filter(s => applies2(s))
    mases.count(l => mases.exists(r => l.entries.tail.head == r.entries.tail.head && l != r)) / 2
  }

  private def applies(s: Path) = {
    s.entries.map(_.x).distinct.size == 1 ||
    s.entries.map(_.y).distinct.size == 1 ||
    s.entries.head.dist(s.entries.reverse.head) == 6
  }
  private def applies2(s: Path) = {
    s.entries.head.dist(s.entries.reverse.head) == 4
  }

  private def next(b: Board[Char], starts: Seq[Path], c: Char) = {
    starts.foldLeft[Seq[Path]](Seq()) {
      case (acc, elt) =>
        acc ++ b.adj(elt.entries.reverse.head).filter(p => b.value(p) == c).map(elt.add)
    }
  }

  private def getXs(b: Board[Char], c: Char): Seq[Path] = {
    for {
      row <- (0 until b.height)
      col <- (0 until b.width)
      if b.value(Point(col, row)) == c
    } yield {
      Path(Seq(Point(col, row)))
    }
  }
}
