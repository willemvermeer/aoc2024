package util

object SeqUtil {

  def insert[T](elt: T, seq: Seq[T], pos: Int) = {
    val (front, back) = seq.splitAt(pos)
    front ++ Seq(elt) ++ back
  }
}
