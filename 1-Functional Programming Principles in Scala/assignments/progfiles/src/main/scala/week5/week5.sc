def removeAt[T](xs: List[T], n: Int): List[T] = xs match
{
    case List() => xs
    case y :: ys => if (n == 0) ys else y :: removeAt(ys, n-1)
}

removeAt(List('a', 'b', 'c', 'd'), 1)

def removeAt2[T](xs: List[T], n: Int): List[T] = (xs take n) ::: (xs drop n + 1)

removeAt2(List('a', 'b', 'c', 'd'), 1)


def flatten(xs: List[Any]): List[Any] = xs match
{
    case List() => xs
    case x :: xs => x match
    {
        case y :: ys => y :: flatten(ys) ::: flatten(xs)
        case y => y :: flatten(xs)
    }
}

flatten(List(List(1, 1), 2, List(3, List(5, 8))))

def msort(xs: List[Int]): List[Int] =
{
    val n = xs.length / 2
    if (n == 0) xs
    else
    {
        def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match
        {
            case (Nil, ys) => ys
            case (xs, Nil) => xs
            case (x :: xs1, y :: ys1) =>
                if (x < y) x :: merge(xs1, ys)
                else y :: merge(xs, ys1)
        }

        val (lhs, rhs) = xs splitAt n
        merge(msort(lhs), msort(rhs))
    }
}

val nums = List(2, -4, 5, 7, 1)
msort(nums)

def msort2[T](xs: List[T])(lt: (T, T) => Boolean): List[T] =
{
    val n = xs.length / 2
    if (n == 0) xs
    else
    {
        def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match
        {
            case (Nil, ys) => ys
            case (xs, Nil) => xs
            case (x :: xs1, y :: ys1) =>
                if (lt(x, y)) x :: merge(xs1, ys)
                else y :: merge(xs, ys1)
        }

        val (lhs, rhs) = xs splitAt n
        merge(msort2(lhs)(lt), msort2(rhs)(lt))
    }
}

msort2(nums)((x: Int, y: Int) => x < y)

println("---------------------------------------")


val fruits = List("apple", "pineapple", "orange", "banana")

nums filter (x => x > 0)
nums filterNot (x => x > 0)
nums partition (x => x > 0)

nums takeWhile  (x => x > 0)
nums dropWhile  (x => x > 0)


val to_pack_list = List("a", "a", "a", "b", "c", "c", "a")
to_pack_list takeWhile(x => x == "a")

def pack[T](xs: List[T]): List[List[T]] = xs match
{
    case Nil => Nil
    case x :: xs1 =>
        {
            val (pref, suff) = xs span (y => y == x)
            pref :: pack(suff)
        }
}

pack(to_pack_list)

def encode[T](xs: List[List[T]]): List[(T, Int)] = xs match
{
    case Nil => Nil
    case x :: xs1 => (x.head, x.length) :: encode(xs1)
}

encode(pack(to_pack_list))

def encode2[T](xs: List[T]): List[(T, Int)] = pack(xs) map (ys => (ys.head, ys.length))

encode2(to_pack_list)

println("---------------------------------------")

def concat[T](xs: List[T], ys: List[T]): List[T] =
    (xs foldRight ys)(_ :: _)

def concat2[T](xs: List[T], ys: List[T]): List[T] =
    (xs foldRight ys)((z:T, zs:List[T]) => z :: zs)

concat(fruits, to_pack_list)
concat2(fruits, to_pack_list)

//define Map and Length using foldRght

def mapFun[T, U](xs: List[T], f: (T) => U): List[U] =
    (xs foldRight List[U]())((z:T, acc:List[U]) => f(z) :: acc)

def mapFun2[T, U](xs: List[T], f: (T) => U): List[U] =
    (xs foldRight List[U]())(f(_) :: _)

mapFun(nums, ((x: Int ) => x*2))
mapFun2(nums, ((x: Int) => x*2))

def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)((z: T, acc: Int) => acc + 1)

lengthFun(concat(fruits, to_pack_list))

def lengthFun2[T](xs: List[T]): Int =
    (xs foldRight 0)((_, acc) => acc + 1) //probably there is another way...lol

lengthFun2(concat(fruits, to_pack_list))