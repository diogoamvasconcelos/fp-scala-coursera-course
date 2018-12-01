val xs: Array[Int] = Array(1, 2, 3)
xs map (x => 2 * x)

val r: Range = 0 to 10 by 2
println(r.toList)

val hello:String = "Hello World"

val pairs = xs zip hello
val (s1, s2) = pairs.unzip

val hello2 = hello flatMap(c => List('.', c))

xs.sum
xs.max


//e.g: list all combinations of numbers x and y where x is drawn from 1..M and y from 1..N
//m = 10, n = 20
(1 to 10) flatMap (x => (1 to 20 by 2) flatMap(y => List(x, y)))

//scalar/dot product of 2 vectors; sum of all product of correspodings elements of the two vectors

def dot_prod(xs: List[Double], ys: List[Double]): Double =
    ((xs zip ys) map (pair => pair._1 * pair._2)).sum

    //alternative with pattern mathching
def dot_prod2(xs: List[Double], ys: List[Double]): Double =
    ((xs zip ys) map{case (x, y) => x * y}).sum

    // this match case is a simplification of { xy match = case (x, y) => ...    we can omit the 'match' as the compiler will infere it from the 'case'

//IsPrime

def isPrime(n: Int): Boolean =
{
    ((1 to n) filter (x => n % x == 0 )).length == 2
}

isPrime(1)
isPrime(2)
isPrime(4)
isPrime(5)

def isPrime2(n: Int): Boolean =
{
    (2 until n) forall (x => n % x != 0)
}

isPrime2(5)
isPrime2(6)