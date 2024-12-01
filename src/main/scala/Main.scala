import java.io.{BufferedWriter, File, FileWriter}
import scala.io.StdIn.readLine

object Main {
  val BASE = "src/main/scala"
  def main(args: Array[String]): Unit = {
    println("Set up which day (just the number, i.e.: 4")
    val day = readLine()
    setup(day)
  }

  private def setup(day: String): Unit = {
    new File(s"${fDayBase(day)}").mkdirs()
    val cookieValue = getSessionCookie
    val r           =
      requests.get(s"https://adventofcode.com/2024/day/$day/input", headers = Seq(("cookie", s"session=$cookieValue")))
    writeFile(s"${fDayBase(day)}/input1", r.text())
    writeFile(s"${fDayBase(day)}/Day$day.scala", mainDay(day))
  }

  private def mainDay(day: String): String = {
    s"""
      |package day$day
      |import util.ReadFile
      |
      |object Day$day extends ReadFile{
      |  val input1: String = readFile("src/main/scala/day$day/input1")
      |
      |  def main(args: Array[String]): Unit = {
      |    solve1
      |  }
      |  private def solve1 = ???
      |  private def solve2 = ???
      |}
      |""".stripMargin
  }

  private def fDayBase(day: String) = s"$BASE/day$day"

  private def writeFile(filename: String, text: String): Unit = {
    val bw = new BufferedWriter(new FileWriter(new File(filename), true)) // <-- 'true'
    bw.write(text)
    bw.close()
  }

  private def getSessionCookie = System.getenv().get("SESSION_COOKIE")
}
