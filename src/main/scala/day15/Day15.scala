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

  def main(args: Array[String]): Unit = {
    println(solve1(exampleSmall))
    println(solve1(example))
    println(solve1(input))
    // println(solve2(example))
    // println(solve2(input))
  }
  private def solve1(s: String): Int = {
    val (b, moves) = read(s)
    println(b.noSpace)
    val end        = moves.foldLeft[Board[Char]](b) {
      case (b, move) =>
//        println(move)
        val next = doMove(b, move)
//        println(next.noSpace)
        next
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
      case '<' =>
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
  private def robot(b: Board[Char]): Point =
    b.all.find(p => b.value(p) == '@').get

  private def read(s: String) = {
    val spl = s.trim().split("\n\n")
    (Board(spl(0).trim()), spl(1).split("\n").mkString.toCharArray.toSeq)
  }
  private def solve2(s: String): Int       = ???
}
