package day10
import util.{Board, Path, ReadFile}

object Day10 extends ReadFile {
  val input: String   = readFile("src/main/scala/day10/input")
  val example: String = """89010123
                          |78121874
                          |87430965
                          |96549874
                          |45678903
                          |32019012
                          |01329801
                          |10456732""".stripMargin
  def main(args: Array[String]): Unit = {
    println(solve1(example))
    println(solve1(input))
    println(solve2(example))
    println(solve2(input))
  }
  private def solve1(s: String) = {
    val b      = Board(s)
    val starts = b.all.filter(p => b.value(p) == '0').map(p => Path(Seq(p)))
    val paths  = (1 to 9).foldLeft[Seq[Path]](starts) {
      case (acc, i) =>
        acc.flatMap { p =>
          b.adjHV(p.entries.reverse.head)
            .filter(p2 => b.value(p2) == i.toString.toCharArray.head)
            .map(p2 => Path(p.entries :+ p2))
        }
    }
    paths
      .groupBy(_.entries.head)
      .map { case (k, v) => (k, v.map(p => p.entries.reverse.head).distinct.size) }
      .values
      .sum
  }
  private def solve2(s: String) = {
    val b      = Board(s)
    val starts = b.all.filter(p => b.value(p) == '0').map(p => Path(Seq(p)))
    val paths  = (1 to 9).foldLeft[Seq[Path]](starts) {
      case (acc, i) =>
        acc.flatMap { p =>
          b.adjHV(p.entries.reverse.head)
            .filter(p2 => b.value(p2) == i.toString.toCharArray.head)
            .map(p2 => Path(p.entries :+ p2))
        }
    }
    paths.size
  }
}
