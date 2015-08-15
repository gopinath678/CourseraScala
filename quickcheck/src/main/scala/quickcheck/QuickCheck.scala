package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

// Group the heap properties.
abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  // forAll takes a function as a parameter
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // conditional property (a > b) for all test cases.
  // ==> is the implication operator
  property("min2") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    findMin(h2) == List(a,b).min
  }

  property("checkEmpty") = forAll { a: Int =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    isEmpty(h2) == true
  }

  property("MergeMin") = forAll { (a: H, b: H) =>
    //val c = meld(a, b)
    //findMin(c) == findMin(a) || findMin(c) == findMin(b)
   findMin(meld(a,b)) == List(findMin(a), findMin(b)).min
  }

  property("CheckSorted") = forAll { a: H =>
    def delete(a: H) : List[Int] = {
      if (isEmpty(a)) Nil
      else findMin(a) :: delete(deleteMin(a))
    }
    val l = delete(a)
    l == l.sorted
  }

  // I am not sure why this is not covered by checksorted???
  // if you insert any 3 elements into an empty heap and delete the minimum,
  // the next minimum should be the second largest element
  property("delete") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(c, insert(b, insert(a, empty)))
    findMin(deleteMin(h)) == List(a, b, c).sorted.drop(1).head
  }

  /*
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }

  lazy val genMap: Gen[Map[Int,Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int,Int]), genMap)
  } yield m.updated(k, v)
  */

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- Gen.frequency((1, empty), (2, genHeap))
  } yield insert(v, h)

  // Define Arbitrary for Heap, so that we can use forAll.
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
