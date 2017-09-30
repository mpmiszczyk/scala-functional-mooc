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


  def squareList1(xs: List[Int]): List[Int] =
    xs match {
      case Nil => Nil
      case y :: ys => y*y :: squareList1(ys)
    }

  def squareList2(xs: List[Int]): List[Int] =
    xs map (x => x*x)


  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (same, rest) = xs span (y => y == x)
      same :: pack(rest)
  }

  def encode[T](list: List[T]): List[(T, Int)] =
    pack (list) map (listOfsame => (listOfsame.head, listOfsame.length))
}
