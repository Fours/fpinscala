package fpinscala.datastructures

object Test {

  def main(args: Array[String]): Unit = {
    tail
    setHead
    drop
    dropWhile
    init
    length // 3.9
    foldLeft // 3.10
    ex_3_11
    ex_3_12
    ex_3_13
    ex_3_14
    ex_3_15
    ex_3_16
    ex_3_17
    ex_3_18
    ex_3_19
    ex_3_20
    ex_3_21
    ex_3_22
    ex_3_23
    ex_3_24
  }

  def tail = {
    assert(List.tail(List(1,2)) == List(2))
    assert(List.tail(List(1)) == Nil)
    assert(List.tail(List(1,2,3)) == List(2,3))
    assert(List.tail(Nil) == Nil)
  }

  def setHead = {
    assert(List.setHead(List(1,2), 2) == List(2,2))
    assert(List.setHead(List(1), 2) == List(2))
    assert(List.setHead(Nil, 2) == List(2))
  }

  def drop: Unit = {
    assert(List.drop(List(1,2),2) == Nil)
    assert(List.drop(List(1,2),3) == Nil)
    assert(List.drop(List(1,2,3),1) == List(2,3))
    assert(List.drop(List(1,2,3),2) == List(3))
    assert(List.drop(List(1,2,3,4),2) == List(3,4))
    assert(List.drop(Nil,1) == Nil)
  }

  def dropWhile = {
    assert(List.dropWhile[Int](List(1,2,3),{_ < 3}) == List(3))
    assert(List.dropWhile[Int](List(1,2,3),{_ == 1}) == List(2,3))
  }

  def init = {
    assert(List.init(List(1,2)) == List(1))
    assert(List.init(List(1,2,3,4)) == List(1,2,3))
  }

  def length = {
    assert(List.length(Nil) == 0)
    assert(List.length(List(1)) == 1)
    assert(List.length(List(1,1)) == 2)
  }

  def foldLeft = {
    assert(List.foldLeft(List(1, 2, 3), 0)(_ + _) == 6)
    assert(List.foldLeft(List("hello ", "world", "!"), "")(_ + _) == "hello world!")
  }

  def ex_3_11 = {
    assert(List.sum3(List(1, 2, 3)) == 6)
    assert(List.sum3(List(1, 2, 3, 1)) == 7)
    assert(List.prod3(List(1, 2, 3)) == 6)
    assert(List.prod3(List(1, 2, 3, 2)) == 12)
    assert(List.length2(List(1, 2, 3)) == 3)
    assert(List.length2(List(1, 2, 3, 1)) == 4)
  }

  def ex_3_12 = {
    assert(List.reverse(List(1,2,3)) == List(3,2,1))
  }

  def ex_3_13 = {
    assert(List.foldRight2(List(1, 2, 3), 0)(_ + _) == 6)
    assert(List.foldRight2(List("hello ", "world", "!"), "")(_ + _) == "hello world!")
    assert(List.foldRight(List("hello ", "world", "!"), "")(_ + _) == "hello world!") // original
  }

  def ex_3_14 = {
    assert(List.append2(List(1), List(2)) == List(1,2))
    assert(List.append2(List(1,2), List(3,4)) == List(1,2,3,4))
    assert(List.append3(List(1), List(2)) == List(1,2))
    assert(List.append3(List(1,2), List(3,4)) == List(1,2,3,4))
  }

  def ex_3_15 = {
    assert(List.concat(List(List(1),List(2))) == List(1,2))
    assert(List.concat(List(List(1,2),List(3,4))) == List(1,2,3,4))
    assert(List.concat2(List(List(1),List(2))) == List(1,2))
    assert(List.concat2(List(List(1,2),List(3,4))) == List(1,2,3,4))
  }

  def ex_3_16 = {
    assert(List.add1(List(1,2)) == List(2, 3))
  }

  def ex_3_17 = {
    assert(List.dubToString(List(1.0,2.0)) == List("1.0", "2.0"))
  }

  def ex_3_18 = {
    assert(List.map(List(1,2))(_ + 1) == List(2, 3))
    assert(List.map(List(1.0,2.0))(_.toString) == List("1.0", "2.0"))
  }

  def ex_3_19 = {
    assert(List.filter(List(1,2,3,4))(_ % 2 == 0) == List(2,4))
  }

  def ex_3_20 = {
    assert(List.flatMap(List(1,2))(List(_)) == List(1,2))
    assert(List.flatMap(List(1,2))(x => List(x, x*10)) == List(1,10,2,20))
  }

  def ex_3_21 = {
    assert(List.filter2(List(1,2,3,4))(_ % 2 == 0) == List(2,4))
  }

  def ex_3_22 = {
    assert(List.addNumbers(List(1,2,3), List(4,5)) == List(5,7,3))
    assert(List.addNumbers(List(1,2), List(3,4,5)) == List(4,6,5))
  }

  def ex_3_23 = {
    assert(List.zipWith(List(1,2,3), List(4,5))(_+_) == List(5,7,3))
    assert(List.zipWith(List("a","b"), List("c","d","e"))(_+"-"+_) == List("a-c","b-d","e"))
  }

  def ex_3_24 = {
    assert(List.hasSubSequence(List(1,2,3), List(1)))
    assert(List.hasSubSequence(List(1,2,3), List(2)))
    assert(List.hasSubSequence(List(1,2,3), List(3)))
    assert(List.hasSubSequence(List(1,2,3), List(1,2)))
    assert(List.hasSubSequence(List(1,2,3), List(2,3)))
    assert(List.hasSubSequence(List(1,2,3), List(1,2,3)))
    assert(List.hasSubSequence(List(1,2,3,4), List(2,3)))

    assert(!List.hasSubSequence(List(1,2,3), List(4)))
    assert(!List.hasSubSequence(List(1,2,3), List(1,2,3,4)))
    assert(!List.hasSubSequence(List(1,2,3), List(1,3)))
    assert(!List.hasSubSequence(List(1,2,3), List(3,4)))
  }


}
