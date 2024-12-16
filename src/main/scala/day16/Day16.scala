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
//    println(solve1(example))
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
        if (currentValue.direction == Up) {
          if (currentValue.score + 1 < unvisited(up).score) {
            unvisited.put(up, unvisited(up).copy(score = currentValue.score + 1, direction = Up))
          }
        } else {
          if (currentValue.score + 1001 < unvisited(up).score) {
            unvisited.put(up, unvisited(up).copy(score = currentValue.score + 1001, direction = Up))
          }
        }
      }
      val down                = Point(nextPoint.x, nextPoint.y + 1)
      if (unvisited.keys.toSeq.contains(down)) {
        if (currentValue.direction == Down) {
          if (currentValue.score + 1 < unvisited(down).score) {
            unvisited.put(down, unvisited(down).copy(score = currentValue.score + 1, direction = Down))
          }
        } else {
          if (currentValue.score + 1001 < unvisited(down).score) {
            unvisited.put(down, unvisited(down).copy(score = currentValue.score + 1001, direction = Down))
          }
        }
      }
      val left                = Point(nextPoint.x - 1, nextPoint.y)
      if (unvisited.keys.toSeq.contains(left)) {
        if (currentValue.direction == Left) {
          if (currentValue.score + 1 < unvisited(left).score) {
            unvisited.put(left, unvisited(left).copy(score = currentValue.score + 1, direction = Left))
          }
        } else {
          if (currentValue.score + 1001 < unvisited(left).score) {
            unvisited.put(left, unvisited(left).copy(score = currentValue.score + 1001, direction = Left))
          }
        }
      }
      val right               = Point(nextPoint.x + 1, nextPoint.y)
      if (unvisited.keys.toSeq.contains(right)) {
        if (currentValue.direction == Right) {
          if (currentValue.score + 1 < unvisited(right).score) {
            unvisited.put(right, unvisited(right).copy(score = currentValue.score + 1, direction = Right))
          }
        } else {
          if (currentValue.score + 1001 < unvisited(right).score) {
            unvisited.put(right, unvisited(right).copy(score = currentValue.score + 1001, direction = Right))
          }
        }
      }
      visited.put(nextPoint, unvisited(nextPoint))
      unvisited.remove(nextPoint)
      println(visited.size)
    }
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
