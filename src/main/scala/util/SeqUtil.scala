package util

object SeqUtil {

  def insert[T](elt: T, seq: Seq[T], pos: Int) = {
    val (front, back) = seq.splitAt(pos)
    front ++ Seq(elt) ++ back
  }
  def swap[T](seq: Seq[T], ind0: Int, ind1: Int) = {
    val v0 = seq(ind0)
    val v1 = seq(ind1)
    seq.updated(ind0, v1).updated(ind1, v0)
  }
}
