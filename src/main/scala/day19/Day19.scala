package day19
import util.ReadFile

import scala.annotation.tailrec

object Day19 extends ReadFile {
  val input: String   = readFile("src/main/scala/day19/input")
  val example: String = """r, wr, b, g, bwu, rb, gb, br
                          |
                          |brwrr
                          |bggr
                          |gbbr
                          |rrbgbr
                          |ubwu
                          |bwurrg
                          |brgr
                          |bbrgwb""".stripMargin

  def main(args: Array[String]): Unit = {
//    println(solve1(example))
    println(solve1(input))
    // println(solve2(example))
    // println(solve2(input))
  }
  private def solve1(s: String): Int = {
    val (patterns, designs) = readInput(s)
    println(patterns.size)
    designs.count { design => println(design); solEx(design, patterns) }
  }
  def doSearch(design: String, patterns: Seq[String]): Seq[Combination] = {
    def inner(current: Combination, remaining: String, target: String): Seq[Combination] = {
      if (remaining.isEmpty)
        if (current.isWinner(target)) {
          println("Found a winner")
          Seq(current)
        } else Nil
      else {
        patterns
          .filter(p => remaining.startsWith(p))
          .sortBy(_.size)
          .foldLeft[Seq[Combination]](Seq()) {
            case (acc, p) =>
              acc ++ inner(Combination(current.patterns :+ p), remaining.substring(p.length), target)
          }
      }
    }
    inner(Combination(Seq()), design, design)
  }
  def doSearch2(design: String, patterns: Seq[String]): Boolean = {
    def inner(current: Combination, remaining: String, target: String, patterns: Seq[String]): Boolean = {
      if (remaining.isEmpty)
        if (current.isWinner(target)) {
          println("Found a winner")
          true
        } else false
      else if (patterns.count(p => remaining.startsWith(p)) == 0) false
      else {
        patterns
          .filter(p => remaining.startsWith(p) && remaining.length >= p.length)
          .foldLeft[Boolean](false) {
            case (acc, p) =>
              val nextRemaining = remaining.substring(p.length)
              acc || inner(Combination(current.patterns :+ p), nextRemaining, target, patterns)
          }
      }
    }
    println(s"Starting with ${patterns.size} patterns max ${patterns.map(_.length).max}")
    inner(Combination(Seq()), design, design, patterns)
  }
  def solEx(design: String, towels: Seq[String]): Boolean = {
    def solutionExists(des: String): Boolean = {
      towels.filter(towel => towel.length <= des.length && des.startsWith(towel)).exists { towel =>
        des == towel || solutionExists(des.drop(towel.length))
      }
    }
    solutionExists(design)
  }
  def doSearch3(designnn: String, patterns: Seq[String]): Boolean = {
    def doIt(design: String): Boolean = {
      patterns
        .filter(p => p.length <= design.length && design.startsWith(p))
        .exists(d =>
          d == design ||
            doIt(design.drop(d.length))
        )
    }
    doIt(designnn)
  }
  private def readInput(s: String): (Seq[String], Seq[String]) = {
    val parts = s.split("\n\n")
    (parts(0).split(", "), parts(1).split("\n"))
  }

  private def solve2(s: String): Int = ???
}
case class Combination(patterns: Seq[String]) {
  def isWinner(target: String) = patterns.mkString == target
}
