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



object Nqueens{
  type Position = (Int, Int)

  def queens(boardSize: Int): Set[List[Position]] = {
    def placeQueens(row: Int): Set[List[Position]] = {
      if (row == 0) Set(List())
      else
        for {
          queens <- placeQueens(row-1)
          column <-  1 to boardSize
          if (queens forall (queen => isSafeFromQueen((column,row), queen)))
        } yield (column, row) :: queens
    }

    placeQueens(boardSize)
  }

  def isSafeFromQueen(position: Position, queen: Position ): Boolean = {
    !sameRow(position, queen) &&
    !sameColumn(position, queen) &&
    !sameDiagonal(position, queen)
  }



  def sameRow(p1: Position, p2: Position): Boolean =
    p1._2 == p2._2


  def sameColumn(p1: Position, p2: Position): Boolean =
    p1._1 == p2._1


  def sameDiagonal(p1: Position, p2: Position): Boolean =
    (math.abs(p1._1 - p1._2) == math.abs(p2._1 - p2._2)) ||
      (p1._1 + p1._2 == p2._1 + p2._2)
}
