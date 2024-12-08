package util

case class Pair[T](l: T, r: T) {
  def flip: Pair[T] = Pair(r, l)
}
