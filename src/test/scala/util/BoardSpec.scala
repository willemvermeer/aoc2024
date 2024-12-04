package util

import org.scalatest.funsuite.AnyFunSuite

class BoardSpec extends AnyFunSuite {

  test("adj left top corner") {
    val b = Board("AB\nBA")
    assert(b.adj(Point(0, 0)).toSet == Set(Point(1, 1), Point(0, 1), Point(1, 0)))
  }
  test("adj right top corner") {
    val b = Board("AB\nBA")
    assert(b.adj(Point(1, 0)).toSet == Set(Point(1, 1), Point(0, 1), Point(0, 0)))
  }
  test("adj middle") {
    val b = Board("ABC\nCAB\nBAC")
    assert(
      b.adj(Point(1, 1)).toSet == Set(
        Point(0, 0),
        Point(1, 0),
        Point(2, 0),
        Point(0, 1),
        Point(2, 1),
        Point(0, 2),
        Point(1, 2),
        Point(2, 2)
      )
    )
  }
}
