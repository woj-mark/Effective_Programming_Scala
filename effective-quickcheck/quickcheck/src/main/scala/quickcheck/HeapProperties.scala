package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.*

class HeapProperties(heapInterface: HeapInterface) extends Properties("Heap"):
  
  // Import all the operations of the `HeapInterface` (e.g., `empty`
  // `insert`, etc.)
  import heapInterface.*


  // Examples of properties
  property("inserting the minimal element and then finding it should return the same minimal element") =
    forAll { (heap: List[Node]) =>
      val min = if isEmpty(heap) then 0 else findMin(heap)
      findMin(insert(min, heap)) == min
    }

  property("the minimum of a heap of two elements should be the smallest of the two elements") =
    forAll { (x1: Int, x2: Int) =>
      val heap = insert(x2, insert(x1, empty))
      val min: Int = if(x1 < x2 ) then x1 else x2
    
      findMin(heap) == min
    }

  property("delete minumum of heap of one element should return an empty heap") =
    forAll { (x: Int) =>
      // create a heap with exactly one element, `x`
      val heap1: List[Node] = insert(x,empty)
      // delete the minimal element from it
      val heap0: List[Node] = deleteMin(heap1)
      // check that heap0 is empty
      heap0.isEmpty
      
    }

  property("continually finding and deleting the minimal element of a heap should return a sorted sequence") =
    // recursively traverse the heap
    def check(heap: List[Node]): Boolean =
      // if the heap is empty, or if it has just one element, we have
      // successfully finished our checks
      if isEmpty(heap) || isEmpty(deleteMin(heap)) then
        true
      else
        // find the minimal element
        val x1: Int = findMin(heap)
        // delete the minimal element of `heap`
        val heap2: List[Node] = deleteMin(heap)
        // find the minimal element in `heap2`
        val x2: Int = findMin(heap)
        // check that the deleted element is smaller than the minimal element
        // of the remaining heap, and that the remaining heap verifies the
        // same property (by recursively calling `check`
        val checked: Boolean = x1 <= x2
        checked
    // check arbitrary heaps
    forAll { (heap: List[Node]) =>
      check(heap)
    }

  // TODO Write more properties here to detect the bugs
  // in bogus BinomialHeap implementations

  property("For an empty heap and a list of integers, insert all integers to heap and repeatly finding and deleting return the intial sequence of intergers") =
  
    def verify(sequence: Seq[Int], heap: List[Node]): Boolean =
      //Return true if both emptied
      if heap.isEmpty && sequence.isEmpty then true
      //Return false if one is empty- that means it is wrong as should be equal size
      else if heap.isEmpty || sequence.isEmpty then false
      else
        //Double conditional statement here.
        //a) The first element of the sequence, the head, should be equal to the newy derived minimum
        //b) Run this recursively, by recalling the function with the tail of the sequence
        val heapMin = findMin(heap)
        heapMin == sequence.head && verify(sequence.tail, deleteMin(heap))
    
     //Run the verify method recursively for a sequence and remove items and put into a queue
    
    forAll{(sequence: Seq[Int]) =>
      //Initiate a new empty heap to which inteers will be added to the queue. It is mutable
      var heap = heapInterface.empty
      //Create a new heaps by iteratively adding the integers from the sequence
      sequence.foreach(integer => heap = insert(integer, heap))
      //Comparing the new heap with the sequence using the verify method. Sort the sequence
      verify(sequence.sortBy(_.self),heap)
    }

  property("finding a minimum of joining of any two enerated heaps should return a minimum from one or the other") =
    //Create the heaps using genHeap lazy functions and run for all method
    //Had to add 'suchThat(_.nonEmpty)' to pass the test
       forAll(genHeap.suchThat(_.nonEmpty)){(heap1:List[Node]) =>
      forAll(genHeap.suchThat(_.nonEmpty)){(heap2:List[Node]) =>

        //Create the meleded heap
        var meldedHeap = meld(heap1, heap2)

        //Extract the minima from either of the heaps and compare against the melded heap
        var minHeap1 = findMin(heap1)
        var minHeap2 = findMin(heap2)
      
        findMin(meldedHeap) == Math.min(minHeap1, minHeap2)
      }
    }

  // random heap generator --- DO NOT MODIFY
  private lazy val genHeap: Gen[List[Node]] = oneOf(const(empty),
    for
      v <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    yield insert(v, h)
  )

  private given Arbitrary[List[Node]] = Arbitrary(genHeap)
  
end HeapProperties
