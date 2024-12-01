package util

import scala.io.Source

trait ReadFile {

  def readFile(filename: String): String = {
    val bufferedSource = Source.fromFile(filename)
    val result         = bufferedSource.getLines.fold("") { case (content, line) => content + line + "\n" }
    bufferedSource.close
    result
  }
}
