package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {r <- arbitrary[Int]; h <- genHeap} yield insert(r, h),
    for {r <- arbitrary[Int]; h <- genHeap} yield insert(r, h),
    for {r <- arbitrary[Int]; h <- genHeap} yield insert(r, h)
  )


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("twoNumbersGenMin") = forAll { (a: Int, b: Int) =>
    val min = List(a,b).min
    val h = insert(a, insert(b, empty))
    findMin(h) == min
  }

  property("addOneElementThenDeleteResultsInEmpty") = forAll { a: Int =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("sorted") = forAll { (h: H) =>

    def checkSorted(previousMin: Int, h: H): Boolean = {
      if (isEmpty(h)) {
        true
      } else {
        val newMin = findMin(h)
        if (previousMin <= newMin) {
          checkSorted(newMin, deleteMin(h))
        } else {
          false
        }
      }
    }

    if (!isEmpty(h))
      checkSorted(findMin(h), h)
    else
      true

  }

  property("meld min") = forAll { (h1: H, h2: H) =>

    val h = meld(h1, h2)

    (isEmpty(h1), isEmpty(h2)) match {
      case (false, false) =>
        val min = findMin(h)
        min == findMin(h1) | min == findMin(h2)
      case (true, false) =>
        val min = findMin(h)
        min == findMin(h2)
      case (false, true) =>
        val min = findMin(h)
        min == findMin(h1)
      case (true, true) => isEmpty(h)
    }

  }



}
