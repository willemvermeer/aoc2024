
package day5
import util.{Pair, ReadFile, SeqUtil}


object Day5 extends ReadFile{
  val example =
    """47|53
      |97|13
      |97|61
      |97|47
      |75|29
      |61|13
      |75|53
      |29|13
      |97|29
      |53|29
      |61|53
      |97|53
      |61|29
      |47|13
      |75|47
      |97|75
      |47|61
      |75|61
      |47|29
      |75|13
      |53|13
      |
      |75,47,61,53,29
      |97,61,53,29,13
      |75,29,13
      |75,97,47,61,53
      |61,13,29
      |97,13,75,29,47
      |""".stripMargin

  val input1: String = readFile("src/main/scala/day5/input1")

  def main(args: Array[String]): Unit = {
    println(solve1(example))
    println(solve1(input1))
    println(solve2(example))
//    println(solve2(input1))
  }
  private def ruleOk(s: Seq[Int], applicableRules: Seq[Pair[Int]]): Boolean = {
    s.dropRight(1).forall { page =>
      applicableRules.filter(_.l == page).forall(p => s.indexOf(page) < s.indexOf(p.r)) &&
        applicableRules.filter(_.r == page).forall(p => s.indexOf(page) > s.indexOf(p.l))
    }
  }
  private def solve1(s: String) = {
    val (rules, updates) = readInput(s)
    val correctUpdates = updates.filter { update =>
      val applicableRules = rules.filter(p => update.contains(p.l) && update.contains(p.r))
      ruleOk(update, applicableRules)
    }
    correctUpdates.map(seq => seq(seq.size / 2)).sum
  }
  private def readInput(s: String) = {
    val top = s.split("\n\n")
    val top1 = top(0).split("\n").map(_.split('|').map(_.toInt).toSeq).toSeq.map(s => Pair(s.head, s.tail.head))
    val top2 = top(1).split("\n").map(_.split(',').map(_.toInt).toSeq).toSeq
    (top1, top2)
  }

  private def solve2(s: String) = {
    val (rules, updates) = readInput(s)
    val incorrectUpdates = updates.filter { update =>
      val applicableRules = rules.filter(p => update.contains(p.l) && update.contains(p.r))
      !ruleOk(update, applicableRules)
    }
    incorrectUpdates.map { update =>
      println(update)
      val applicableRules = rules.filter(p => update.contains(p.l) && update.contains(p.r))
      val reshuffle = update.tail.foldLeft[Seq[Int]](Seq(update.head)) { case (acc, page) =>
        println(acc + ":" + page)
        (0 to acc.size).find{i =>
          val newseq = SeqUtil.insert(page, acc, i)
          println(newseq)
          ruleOk(newseq, applicableRules)} match {
          case Some(i) => SeqUtil.insert(page, acc, i)
          case None    => println("Cannot happen")
          Seq()
        }
      }
      println(reshuffle)
    }
  }


}
