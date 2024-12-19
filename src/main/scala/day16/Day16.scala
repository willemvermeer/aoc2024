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
//    println(solve1(input))
    println(solve2(example))
    println(solve2(example2))
    println(solve2(input))
  }
  // 490 too low
  // 500 incorrect
  // 507 too high
  private def next(mmap: mutable.Map[Point, State]) = {
    val min = mmap.values.map(_.score).min
    mmap.filter(_._2.score == min).keys.head
  }
  private def solve1(s: String): Int = {
    val b      = read(s)
    val target = b.all.find(p => b.value(p) == 'E').get
    doIt(b)(target).score
  }
  def doIt(b: Board[Char]): mutable.Map[Point, State] = {
    val start                                = b.all.find(p => b.value(p) == 'S').get
    val target                               = b.all.find(p => b.value(p) == 'E').get
//    println(target)
    val visited                              = mutable.Map[Point, State]()
    val unvisited: mutable.Map[Point, State] = mutable.Map()
    b.all
      .filterNot(p => Seq('#').contains(b.value(p)))
      .foreach(p => unvisited.put(p, State(Seq(), Int.MaxValue, Right)))
    unvisited.put(start, State(Seq(start), 0, Right))
    var nextPoint                            = start
    while (unvisited.nonEmpty) {
      nextPoint = next(unvisited)
//      if (nextPoint == Point(4,7) || nextPoint == Point(5,8)) {
//        println("boe")
//      }
      val currentValue: State = unvisited(nextPoint)
      val up                  = Point(nextPoint.x, nextPoint.y - 1)
      val nextScoreUp         = if (currentValue.direction == Up) currentValue.score + 1 else currentValue.score + 1001
      if (unvisited.keys.toSeq.contains(up)) {
        if (nextScoreUp < unvisited(up).score) {
          unvisited.put(
            up,
            unvisited(up).copy(score = nextScoreUp, direction = Up, history = currentValue.history :+ up)
          )
        }
      } else if (visited.contains(up) && nextScoreUp == visited(up).score + 1000) {
//        println("UPPP")
        if (up != target)
          visited.put(up, visited(up).copy(alternatives = visited(up).alternatives :+ currentValue.history))
      }
      val down                = Point(nextPoint.x, nextPoint.y + 1)
      val nextScoreDown       = if (currentValue.direction == Down) currentValue.score + 1 else currentValue.score + 1001
      if (unvisited.keys.toSeq.contains(down)) {
        if (nextScoreDown < unvisited(down).score) {
          unvisited.put(
            down,
            unvisited(down).copy(score = nextScoreDown, direction = Down, history = currentValue.history :+ down)
          )
        }
      } else if (visited.contains(down) && nextScoreDown == visited(down).score + 1000) {
//        println("UDOWBN")
        if (down != target)
          visited.put(down, visited(down).copy(alternatives = visited(down).alternatives :+ currentValue.history))
      }
      val left                = Point(nextPoint.x - 1, nextPoint.y)
      val nextScoreLeft       = if (currentValue.direction == Left) currentValue.score + 1 else currentValue.score + 1001
      if (unvisited.keys.toSeq.contains(left)) {
        if (nextScoreLeft < unvisited(left).score) {
          unvisited.put(
            left,
            unvisited(left).copy(score = nextScoreLeft, direction = Left, history = currentValue.history :+ left)
          )
        }
      } else if (visited.contains(left) && nextScoreLeft == visited(left).score + 1000) {
//        println("LEFT")
        if (left != target)
          visited.put(left, visited(left).copy(alternatives = visited(left).alternatives :+ currentValue.history))
      }
      val right               = Point(nextPoint.x + 1, nextPoint.y)
      val nextScoreRight      = if (currentValue.direction == Right) currentValue.score + 1 else currentValue.score + 1001
      if (unvisited.keys.toSeq.contains(right)) {
        if (nextScoreRight < unvisited(right).score) {
          unvisited.put(
            right,
            unvisited(right).copy(score = nextScoreRight, direction = Right, history = currentValue.history :+ right)
          )
        }
      } else if (visited.contains(right) && nextScoreRight == visited(right).score + 1000) {
//        println("RIGHT")
        if (right != target)
          visited.put(right, visited(right).copy(alternatives = visited(right).alternatives :+ currentValue.history))
      }
      visited.put(nextPoint, unvisited(nextPoint))
      unvisited.remove(nextPoint)
//      println(visited.size)
    }
//    println(visited(target).history)
    visited
  }
  private def read(s: String) = Board(s)

  private def solve2(s: String): Int = {
    val b      = read(s)
    val start  = b.all.find(p => b.value(p) == 'S').get
    val target = b.all.find(p => b.value(p) == 'E').get
    val all    = b.all.filter(p => b.value(p) != '#')
    val res    = doIt(b)
    val answer = res(target).history.flatMap(p => p +: res(p).alternatives.flatten.distinct).distinct
    val allll  = (res(target).history.reverse.sliding(2).foldLeft[Seq[Point]](Seq()) {
      case (acc, seq) =>
        val p1 = seq.head
        val p2 = seq.reverse.head
        val nn = p2.neighbours(all).filterNot(_ == p1)
        val nb = nn.map(res(_)).map(_.score)
//      println(p1 + ":" + p2 + ";" + nb.mkString(","))
//      println(res(p1). + ":" + res(p2))
        if (nb.size == 2) {
          if (opposite(nn.map(res(_)).map(_.direction)) && nb.head == nb.reverse.head) {
            acc ++ res(nn.head).history ++ res(nn.reverse.head).history
          } else if (!opposite(nn.map(res(_)).map(_.direction)) && Math.abs(nb.head - nb.reverse.head) == 1000) {
            acc ++ res(nn.head).history ++ res(nn.reverse.head).history
          } else acc :+ p2
        } else acc :+ p2
//      if (nb.size == 2 && (Math.abs(nb.head - nb.reverse.head) == 1000 || nb.head == nb.reverse.head)) {
//        println("beide")
//        acc ++ res(nn.head).history ++ res(nn.reverse.head).history
//      }

    } ++ Seq(start, target)).distinct
    println(allll.size)
    allll.size
    println(allll.foldLeft[Board[Char]](b) { case (acc, elt) => acc.update('O', elt) }.noSpace)
//    1 + res(target).history.flatMap(p => p +: res(p).alternatives.flatten.distinct).distinct.size
    allll.size
  }

  private def opposite(nb: Seq[Direction]) = nb.toSet == Set(Up, Down) || nb.toSet == Set(Left, Right)
}
sealed trait Direction
case object Up    extends Direction
case object Down  extends Direction
case object Left  extends Direction
case object Right extends Direction
case class State(history: Seq[Point], score: Int, direction: Direction, alternatives: Seq[Seq[Point]] = Seq())
