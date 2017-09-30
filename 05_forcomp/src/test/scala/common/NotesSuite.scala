package notes

import org.scalatest._


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
object NotesTests extends FunSuite {
  import Session._

  test("sorting with mergeSort"){
    def msInt(list: List[Int]): List[Int] = mergeSort(list)((x, y) => x < y)

    assert(msInt(List()) === List())
    assert(msInt(List(1,2,3,4)) === List(1,2,3,4))
    assert(msInt(List(12,5,11)) === List(5,11,12))
    assert(msInt(List(3,0,-4,110)) === List(-4,0,3,110))
  }

}
