package util

import org.scalatest.funsuite.AnyFunSuite

class SeqUtilSpec extends AnyFunSuite {

  test("insert into empty") {
    assert(SeqUtil.insert(1, Seq(), 0) == Seq(1))
  }
  test("insert at start") {
    assert(SeqUtil.insert(1, Seq(2, 3), 0) == Seq(1, 2, 3))
  }
  test("insert at end") {
    assert(SeqUtil.insert(1, Seq(2), 1) == Seq(2, 1))
  }
  test("insert at middle") {
    assert(SeqUtil.insert(1, Seq(2, 3), 1) == Seq(2, 1, 3))
  }
  test("swap two elts") {
    assert(SeqUtil.swap(Seq(1, 2), 0, 1) == Seq(2, 1))
  }
  test("swap two elts of three") {
    assert(SeqUtil.swap(Seq(1, 2, 3), 0, 1) == Seq(2, 1, 3))
  }
  test("swap two elts of three take 2") {
    assert(SeqUtil.swap(Seq(1, 2, 3), 0, 2) == Seq(3, 2, 1))
  }
  test("swap two elts of four") {
    assert(SeqUtil.swap(Seq(1, 2, 3, 4), 1, 2) == Seq(1, 3, 2, 4))
  }
}
