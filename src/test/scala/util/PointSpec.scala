package util

import org.scalatest.funsuite.AnyFunSuite

class PointSpec extends AnyFunSuite {

  test("one") {
    val p = Point(0, 1)
    assert(Point.permutations(Seq(p)) == Seq())
  }
  test("two") {
    val p  = Point(0, 1)
    val p2 = Point(1, 1)
    assert(Point.permutations(Seq(p, p2)) == Seq(Pair(p, p2)))
  }
  test("three") {
    val p  = Point(0, 1)
    val p2 = Point(1, 1)
    val p3 = Point(1, 2)
    assert(
      Point.permutations(Seq(p, p2, p3)).toSet ==
        Seq(Pair(p, p2), Pair(p, p3), Pair(p2, p3)).toSet
    )
  }
  test("four") {
    val p  = Point(0, 1)
    val p2 = Point(1, 1)
    val p3 = Point(1, 2)
    val p4 = Point(1, 4)
    assert(
      Point.permutations(Seq(p, p2, p3, p4)).toSet ==
        Set(Pair(p, p2), Pair(p, p3), Pair(p2, p3), Pair(p, p4), Pair(p2, p4), Pair(p3, p4))
    )
  }

}
