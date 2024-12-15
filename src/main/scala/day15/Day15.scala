package day15
import util.{Board, Point, ReadFile}

object Day15 extends ReadFile {
  val input: String        = readFile("src/main/scala/day15/input")
  val exampleSmall: String = """########
                               |#..O.O.#
                               |##@.O..#
                               |#...O..#
                               |#.#.O..#
                               |#...O..#
                               |#......#
                               |########
                               |
                               |<^^>>>vv<v>>v<<""".stripMargin
  val example: String      = """##########
                          |#..O..O.O#
                          |#......O.#
                          |#.OO..O.O#
                          |#..O@..O.#
                          |#O#..O...#
                          |#O..O..O.#
                          |#.OO.O.OO#
                          |#....O...#
                          |##########
                          |
                          |<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
                          |vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
                          |><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
                          |<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
                          |^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
                          |^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
                          |>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
                          |<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
                          |^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
                          |v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^""".stripMargin

  val example2 = """#######
                   |#...#.#
                   |#.....#
                   |#..OO@#
                   |#..O..#
                   |#.....#
                   |#######
                   |
                   |<vv<<^^<<^^""".stripMargin
  def main(args: Array[String]): Unit = {
//    println(solve1(exampleSmall))
//    println(solve1(example))
//    println(solve1(input))
//    println(solve2(example))
    println(solve2(input))
  }
  private def solve1(s: String): Int = {
    val (b, moves) = read(s)
    println(b.noSpace)
    val end        = moves.foldLeft[Board[Char]](b) {
      case (b, move) =>
        doMove(b, move)
    }
    println(end.noSpace)
    end.all.filter(p => end.value(p) == 'O').map(p => (100 * p.y) + p.x).sum
  }

  private def doMove(b: Board[Char], move: Char): Board[Char] = {
    val rob: Point = robot(b)
    move match {
      case '^' =>
        val candidate = Point(rob.x, rob.y - 1)
        if (b.outside(candidate) || b.value(candidate) == '#') b
        else if (b.value(candidate) == '.') b.swap(rob, candidate)
        else {
          // it's a boulder - push until stuck
          (1 until (candidate.y)).findLast(y => Seq('.', '#').contains(b.value(rob.x, y))) match {
            case Some(pnt) if b.value(candidate.x, pnt) == '.' =>
              (pnt to candidate.y).foldLeft(b) {
                case (b, c) =>
                  b.swap(Point(rob.x, c), Point(rob.x, c + 1))
              }
            case _                                             => b
          }
        }
      case '>' =>
        doRight(b, rob)
      case '<' =>
        doLeft(b, rob)
      case 'v' =>
        val candidate = Point(rob.x, rob.y + 1)
        if (b.outside(candidate) || b.value(candidate) == '#') b
        else if (b.value(candidate) == '.') b.swap(rob, candidate)
        else {
          // it's a boulder - push until stuck
          (candidate.y + 1 until b.height - 1).find(y => Seq('.', '#').contains(b.value(rob.x, y))) match {
            case Some(pnt) if b.value(rob.x, pnt) == '.' =>
              (rob.y + 1 to pnt).reverse.foldLeft(b) {
                case (b, c) =>
                  b.swap(Point(rob.x, c), Point(rob.x, c - 1))
              }
            case _                                       => b
          }
        }
      case _   => b
    }
  }

  private def doLeft(b: Board[Char], rob: Point) = {
    val candidate = Point(rob.x - 1, rob.y)
    if (b.outside(candidate) || b.value(candidate) == '#') b
    else if (b.value(candidate) == '.') b.swap(rob, candidate)
    else {
      // it's a boulder - push until stuck
      (1 until candidate.x).findLast(x => Seq('.', '#').contains(b.value(x, rob.y))) match {
        case Some(pnt) if b.value(pnt, rob.y) == '.' =>
          (pnt until rob.x).foldLeft(b) {
            case (b, c) =>
              b.swap(Point(c, rob.y), Point(c + 1, rob.y))
          }
        case _                                       => b
      }
    }
  }

  private def doRight(b: Board[Char], rob: Point) = {
    val candidate = Point(rob.x + 1, rob.y)
    if (b.outside(candidate) || b.value(candidate) == '#') b
    else if (b.value(candidate) == '.') b.swap(rob, candidate)
    else {
      // it's a boulder - push until stuck
      (candidate.x + 1 until b.width - 1).find(x => Seq('.', '#').contains(b.value(x, rob.y))) match {
        case Some(pnt) if b.value(pnt, rob.y) == '.' =>
          (candidate.x to pnt).reverse.foldLeft(b) {
            case (b, c) =>
              b.swap(Point(c, rob.y), Point(c - 1, rob.y))
          }
        case _                                       => b
      }
    }
  }

  private def robot(b: Board[Char]): Point =
    b.all.find(p => b.value(p) == '@').get

  private def read(s: String): (Board[Char], Seq[Char]) = {
    val spl = s.trim().split("\n\n")
    (Board(spl(0).trim()), spl(1).split("\n").mkString.toCharArray.toSeq)
  }
  private def doMove2(b: Board[Char], move: Char): Board[Char] = {
    val rob: Point = robot(b)
    move match {
      case '>' =>
        doRight(b, rob)
      case '<' =>
        doLeft(b, rob)
      case 'v' =>
        val candidate = Point(rob.x, rob.y + 1)
        if (b.outside(candidate) || b.value(candidate) == '#') b
        else if (b.value(candidate) == '.') b.swap(rob, candidate)
        else if (b.value(candidate) == '[') {
          // per rij bijhouden welke boxes er zitten
          var start  = Seq(Seq(Point(candidate.x, candidate.y), Point(candidate.x + 1, candidate.y)))
          var index  = candidate.y
          var points = start.head
          while (index < b.height && !(blocked(points, b, 'v') || unblocked(points, b, 'v'))) {
            index = index + 1
            start = calcNextRow(points, b, 'v') +: start
            points = start.head
          }
          if (unblocked(start.head, b, 'v')) {
            start
              .foldLeft[Board[Char]](b) {
                case (acc, elt) =>
                  elt.foldLeft[Board[Char]](acc) {
                    case (acc2, p) =>
                      acc2.swap(p, Point(p.x, p.y + 1))
                  }
              }
              .swap(rob, Point(rob.x, rob.y + 1))
          } else b
        } else {
          // per rij bijhouden welke boxes er zitten
          var start  = Seq(Seq(Point(candidate.x, candidate.y), Point(candidate.x - 1, candidate.y)))
          var index  = candidate.y
          var points = start.head
          while (index < b.height && !(blocked(points, b, 'v') || unblocked(points, b, 'v'))) {
            index = index + 1
            start = calcNextRow(points, b, 'v') +: start
            points = start.head
          }
          if (unblocked(start.head, b, 'v')) {
            start
              .foldLeft[Board[Char]](b) {
                case (acc, elt) =>
                  elt.foldLeft[Board[Char]](acc) {
                    case (acc2, p) =>
                      acc2.swap(p, Point(p.x, p.y + 1))
                  }
              }
              .swap(rob, Point(rob.x, rob.y + 1))
          } else b
        }
      case '^' =>
        val candidate = Point(rob.x, rob.y - 1)
        if (b.outside(candidate) || b.value(candidate) == '#') b
        else if (b.value(candidate) == '.') b.swap(rob, candidate)
        else if (b.value(candidate) == '[') {
          // per rij bijhouden welke boxes er zitten
          var start  = Seq(Seq(Point(candidate.x, candidate.y), Point(candidate.x + 1, candidate.y)))
          var index  = candidate.y
          var points = start.head
          while (index > 0 && !(blocked(points, b, '^') || unblocked(points, b, '^'))) {
            index = index - 1
            start = calcNextRow(points, b, '^') +: start
            points = start.head
          }
          if (unblocked(start.head, b, '^')) {
            start
              .foldLeft[Board[Char]](b) {
                case (acc, elt) =>
                  elt.foldLeft[Board[Char]](acc) {
                    case (acc2, p) =>
                      acc2.swap(p, Point(p.x, p.y - 1))
                  }
              }
              .swap(rob, Point(rob.x, rob.y - 1))
          } else b
        } else {
          // per rij bijhouden welke boxes er zitten
          var start  = Seq(Seq(Point(candidate.x, candidate.y), Point(candidate.x - 1, candidate.y)))
          var index  = candidate.y
          var points = start.head
          while (index > 0 && !(blocked(points, b, '^') || unblocked(points, b, '^'))) {
            index = index - 1
            start = calcNextRow(points, b, '^') +: start
            points = start.head
          }
          if (unblocked(start.head, b, '^')) {
            start
              .foldLeft[Board[Char]](b) {
                case (acc, elt) =>
                  elt.foldLeft[Board[Char]](acc) {
                    case (acc2, p) =>
                      acc2.swap(p, Point(p.x, p.y - 1))
                  }
              }
              .swap(rob, Point(rob.x, rob.y - 1))
          } else b
        }
      case _   => b
    }
  }
  def blocked(points: Seq[Point], b: Board[Char], direction: Char): Boolean = {
    if (direction == 'v') {
      points.map(p => Point(p.x, p.y + 1)).count(p => b.value(p) == '#' || b.outside(p)) > 0
    } else {
      points.map(p => Point(p.x, p.y - 1)).count(p => b.value(p) == '#' || b.outside(p)) > 0
    }
  }
  def unblocked(points: Seq[Point], b: Board[Char], direction: Char): Boolean = {
    if (direction == 'v') {
      points.map(p => Point(p.x, p.y + 1)).forall(p => b.value(p) == '.')
    } else {
      points.map(p => Point(p.x, p.y - 1)).forall(p => b.value(p) == '.')
    }
  }
  def calcNextRow(points: Seq[Point], b: Board[Char], direction: Char): Seq[Point] = {
    val minx  = points.map(_.x).min
    val maxx  = points.map(_.x).max
    val y     = points.head.y
    val nexty = if (direction == 'v') y + 1 else y - 1
    (if (b.value(Point(minx - 1, nexty)) == '[') Seq(Point(minx - 1, nexty)) else Seq()) ++
      (minx to maxx).map(x => Point(x, nexty)).filter(p => Seq('[', ']').contains(b.value(p))) ++
      (if (b.value(Point(maxx + 1, nexty)) == ']') Seq(Point(maxx + 1, nexty)) else Seq())
  }
  private def solve2(s: String): Int = {
    val (b, moves) = read(s)
    val bw         = widen(b)
    println(bw.noSpace)
    val end        = moves.foldLeft[Board[Char]](bw) {
      case (b, move) => doMove2(b, move)
    }
    println(end.noSpace)
    end.all.filter(p => end.value(p) == '[').map(p => (100 * p.y) + p.x).sum
  }

  private def widen(b: Board[Char]) = {
    Board(b.rows.map { row =>
      row
        .map {
          case '#' => "##"
          case 'O' => "[]"
          case '@' => "@."
          case '.' => ".."
        }
        .mkString
        .toCharArray
        .toSeq
    })
  }
}
