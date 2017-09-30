package notes

object Session {

  def mergeSort[T] (list: List[T])(lt: (T,T) => Boolean): List[T] = list match {
    case List() => list
    case List(i) => list
    case _ =>
      def merge(first: List[T], second: List[T]): List[T] =
        (first, second) match {
          case (Nil, second) => second
          case (first, Nil) => first
          case (f :: fs, s :: ss) if (lt(s, f)) =>
            s :: merge(first, ss)
          case (f :: fs, second) =>
            f :: merge(fs, second)
        }

      val middle: Int = list.length / 2
      val (first, second) = list splitAt (middle)
      merge(mergeSort(first)(lt), mergeSort(second)(lt)) // to many `lt`
  }

  

}
