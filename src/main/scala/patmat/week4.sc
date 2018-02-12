def flatten(xs: List[Any]):List[Any] = xs match {
  case (x: List[Any])::rest => flatten(x):::flatten(rest)
  case x::rest => x::flatten(rest)
  case Nil => xs
}

flatten(List(List(1, 1), 2, List(3, List(5, 8))))


def squareList(xs: List[Int]): List[Int] =
  xs match {
    case Nil => xs
    case y :: ys => y * y :: squareList(ys)
  }

def squareListMap(xs: List[Int]): List[Int] =
  xs map (x => x * x)


def stuff = List(1,2,3,4)

squareList(stuff)

squareListMap(stuff)


def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => List()
  case x :: _ => {
    val (first, rest) = xs span (y => y == x)
    first :: pack(rest)
  }
}

def encode[T](xs: List[T]): List[(T,Int)] = {
  val packed = pack(xs)
  packed map (xs => (xs.head, xs.length))
}

val chars: List[Char] = List('a', 'a', 'a', 'b', 'c', 'c', 'a')
pack(chars)
encode(chars)

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( (x, ys) => f(x) :: ys )

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((_, x) => x + 1 )

lengthFun(chars)
mapFun[Char, Int](chars, _.toInt)


