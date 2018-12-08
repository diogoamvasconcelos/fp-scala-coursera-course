def from(n: Int): Stream[Int] = n #:: from(n + 1)

val nats = from(0)
val m4s = nats map (_ * 4)

(m4s take 1000).toList

//calculate prime numbers, using the sieve of erastosthenes!!!
def sieve(s: Stream[Int]): Stream[Int] =
    s.head #:: sieve(s.tail filter (_ % s.head != 0)) //take first number, which is a prime and follow with all the other numbers that are not its multiples

val primes = sieve(from(2))

(primes take 1000).toList


def sqrtStream(x: Double): Stream[Double] = {
    def improve(guess: Double) = (guess + x / guess) / 2
    lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
    guesses
}

sqrtStream(2)

(sqrtStream(2) take 10).toList