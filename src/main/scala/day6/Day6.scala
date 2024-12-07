package day6
import util.{Board, Point, ReadFile}

import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable

sealed trait Direction {
  def next: Direction
}
case object Up    extends Direction {
  override def next = Right
}
case object Right extends Direction {
  override def next = Down
}
case object Down  extends Direction {
  override def next = Left
}
case object Left  extends Direction {
  override def next = Up
}
case class State(pos: Point, d: Direction)

object Day6 extends ReadFile {
  val input1: String  = readFile("src/main/scala/day6/input1")
  val example: String =
    """....#.....
      |.........#
      |..........
      |..#.......
      |.......#..
      |..........
      |.#..^.....
      |........#.
      |#.........
      |......#...""".stripMargin
  def main(args: Array[String]): Unit = {
    println(solve1(example))
    println(solve1(input1))
    println(solve2(example))
    println(solve2(input1))
  }
  private def next(s: State, b: Board[Char]): State = {
    val potential = s.d match {
      case Up    => Point(s.pos.x, s.pos.y - 1)
      case Down  => Point(s.pos.x, s.pos.y + 1)
      case Right => Point(s.pos.x + 1, s.pos.y)
      case Left  => Point(s.pos.x - 1, s.pos.y)
    }
    if (b.outside(potential)) State(potential, s.d)
    else {
      if (b.value(potential) == '#')
        State(s.pos, s.d.next)
      else State(potential, s.d)
    }
  }
  private def solve1(s: String) = {
    val b: Board[Char]    = Board(s)
    val locatorPos: Point = b.position('^')
    var state             = State(locatorPos, Up)
    var visited           = Seq[Point]()
    while (!b.outside(state.pos)) {
      visited = visited :+ state.pos
      state = next(state, b)
    }
    visited.distinct.size
  }
  private def solve2(s: String) = {
    val b: Board[Char]    = Board(s)
    val locatorPos: Point = b.position('^')
    val state             = State(locatorPos, Up)
    ((for {
      row <- (0 until b.height)
      col <- (0 until b.width)
      if !(row == locatorPos.y && col == locatorPos.x)
    } yield {
      Point(col, row)
    }).toList).par
      .map { p =>
        val bnew = b.update('#', p)
        isEndLess(state, bnew)
      }
      .count(_ == true)
  }
  private def isEndLess(s: State, b: Board[Char]) = {
    var visited   = Seq[State]()
    var state     = s
    var endlessLp = false
    while (!b.outside(state.pos) && !endlessLp) {
      if (visited.contains(state)) {
        endlessLp = true
      } else {
        visited = visited :+ state
        state = next(state, b)
      }
    }
    endlessLp
  }
}
