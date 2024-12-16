package day16
import util.{Board, Point, ReadFile}

import scala.collection.mutable

object Day16      extends ReadFile {
  val input: String   = readFile("src/main/scala/day16/input")
  val example: String = """###############
                          |#.......#....E#
                          |#.#.###.#.###.#
                          |#.....#.#...#.#
                          |#.###.#####.#.#
                          |#.#.#.......#.#
                          |#.#.#####.###.#
                          |#...........#.#
                          |###.#.#####.#.#
                          |#...#.....#.#.#
                          |#.#.#.###.#.#.#
                          |#.....#...#.#.#
                          |#.###.#.#.#.#.#
                          |#S..#.....#...#
                          |###############""".stripMargin

  val example2                = """#################
                   |#...#...#...#..E#
                   |#.#.#.#.#.#.#.#.#
                   |#.#.#.#...#...#.#
                   |#.#.#.#.###.#.#.#
                   |#...#.#.#.....#.#
                   |#.#.#.#.#.#####.#
                   |#.#...#.#.#.....#
                   |#.#.#####.#.###.#
                   |#.#.#.......#...#
                   |#.#.###.#####.###
                   |#.#.#...#.....#.#
                   |#.#.#.#####.###.#
                   |#.#.#.........#.#
                   |#.#.#.#########.#
                   |#S#.............#
                   |#################""".stripMargin
  def main(args: Array[String]): Unit = {
    println(solve1(example))
//    println(solve1(example2))
    println(solve1(input))
    // println(solve2(example))
    // println(solve2(input))
  }
  private def next(mmap: mutable.Map[Point, State]) = {
    val min = mmap.values.map(_.score).min
    mmap.filter(_._2.score == min).keys.head
  }
  private def solve1(s: String): Int = {
    val b                                    = read(s)
    val start                                = b.all.find(p => b.value(p) == 'S').get
    val target                               = b.all.find(p => b.value(p) == 'E').get
    println(target)
    val visited                              = mutable.Map[Point, State]()
    val unvisited: mutable.Map[Point, State] = mutable.Map()
    b.all
      .filterNot(p => Seq('#').contains(b.value(p)))
      .foreach(p => unvisited.put(p, State(Seq(), Int.MaxValue, Right)))
    unvisited.put(start, State(Seq(start), 0, Right))
    var nextPoint                            = start
    while (unvisited.nonEmpty) {
      nextPoint = next(unvisited)
      val currentValue: State = unvisited(nextPoint)
      val up                  = Point(nextPoint.x, nextPoint.y - 1)
      if (unvisited.keys.toSeq.contains(up)) {
        val nextScore = if (currentValue.direction == Up) currentValue.score + 1 else currentValue.score + 1001
        if (nextScore < unvisited(up).score) {
          unvisited.put(up, unvisited(up).copy(score = nextScore, direction = Up, history = currentValue.history :+ up))
        }
      }
      val down                = Point(nextPoint.x, nextPoint.y + 1)
      if (unvisited.keys.toSeq.contains(down)) {
        val nextScore = if (currentValue.direction == Down) currentValue.score + 1 else currentValue.score + 1001
        if (nextScore < unvisited(down).score) {
          unvisited.put(
            down,
            unvisited(down).copy(score = nextScore, direction = Down, history = currentValue.history :+ down)
          )
        }
      }
      val left                = Point(nextPoint.x - 1, nextPoint.y)
      if (unvisited.keys.toSeq.contains(left)) {
        val nextScore = if (currentValue.direction == Left) currentValue.score + 1 else currentValue.score + 1001
        if (nextScore < unvisited(left).score) {
          unvisited.put(
            left,
            unvisited(left).copy(score = nextScore, direction = Left, history = currentValue.history :+ left)
          )
        }
      }
      val right               = Point(nextPoint.x + 1, nextPoint.y)
      if (unvisited.keys.toSeq.contains(right)) {
        val nextScore = if (currentValue.direction == Right) currentValue.score + 1 else currentValue.score + 1001
        if (nextScore < unvisited(right).score) {
          unvisited.put(
            right,
            unvisited(right).copy(score = nextScore, direction = Right, history = currentValue.history :+ right)
          )
        }
      }
      visited.put(nextPoint, unvisited(nextPoint))
      unvisited.remove(nextPoint)
      println(visited.size)
    }
    println(visited(target).history)
    visited(target).score
  }
  private def read(s: String) = Board(s)

  private def solve2(s: String): Int = ???
}
sealed trait Direction
case object Up    extends Direction
case object Down  extends Direction
case object Left  extends Direction
case object Right extends Direction
case class State(history: Seq[Point], score: Int, direction: Direction)
