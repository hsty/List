
object Exampler extends App {

  def last[T](xs: List[T]): T = xs match {
    case List() => throw new Error("Last of empty list")
    case List(x) => x
    case y :: ys => last(ys)
  }

  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }

  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }

  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => xs
    case y :: ys => reverse(ys) ++ List(y)
  }

  def removeAt[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs drop n+1)

  def flatten(xs: List[Any]): List[Any] = xs match {
    case z :: zs => z match {
      case y::ys => flatten(y::ys) ++ flatten(zs)
      case _ => z :: flatten(zs)
    }
    case Nil => Nil
  }

  println(removeAt(6, List('a','b','c','d')))

  println(flatten(List(List(1, 1), 2, List(List(3,7,8), List(5, 8), List(1)))))

  def msort(xs: List[Int]): List[Int] = {
    val n = xs.length/2
    if(n == 0) xs
    else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] =
        (xs, ys) match {
          case (Nil, zs) => zs
          case (js, Nil) => js
          case (j :: js, z :: zs) => if(j<z) j :: merge(js, ys) else z :: merge(xs, zs)
        }
      val (fst,snd) = xs.splitAt(n)
      merge(msort(fst), msort(snd))
    }
  }



}
