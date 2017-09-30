package notes

object Session {

  def mergeSort (list: List[Int]): List[Int] = list match {
    case List() => list
    case List(i) => list
    case _ =>
      val middle: Int = list.length / 2
      val (first, second) = list splitAt (middle)
      merge(mergeSort(first), mergeSort(second))
  }

  def merge(first: List[Int], second: List[Int]): List[Int] =
    (first, second) match {
      case (Nil, second) => second
      case (first, Nil) => first
      case (f :: fs, s :: ss) if (s < f) =>
        s :: merge(first, ss)
      case (f :: fs, second) =>
        f :: merge(fs, second)
    }

}
