def product(f: Int => Int)(a: Int, b: Int): Int =
  {
    if (a > b) 1 else f(a) * product(f)(a + 1, b)
  }

product((x:Int) => x)(1, 4)

def factorial(a:Int):Int = product((x:Int) => x)(1, a)

factorial(4)

def general_sum_prod(f: Int => Int, init:Int, comb: (Int, Int) => Int)(a: Int, b: Int): Int =
{
  if (a > b) init
  else comb(f(a), general_sum_prod(f, init, comb)(a + 1, b))
}

def factorial2(a:Int):Int = general_sum_prod((x:Int) => x, 1, (x1:Int, x2:Int) => x1 * x2)(1, a)

factorial2(4)


//-----------------------------------------------

val tolerance = 0.0001

def abs(x:Double) = if (x < 0) -x else x

def IsCloseEnough(x: Double, y: Double) : Boolean =
  abs((x -y) / x) / x < tolerance

def FixedPoint(f: Double => Double)(first_guess: Double) =
{
  def Iterate(guess: Double): Double =
  {
    println("guess:" + guess.toString)
    val next = f(guess)
    if (IsCloseEnough(guess, next)) next
    else Iterate(next)
  }

  Iterate(first_guess)
}

FixedPoint((x: Double) => 1 + (x/2))(0) // should be 2

def sqrt(x: Double) = FixedPoint((y: Double) => ( y + x / y) / 2)(1)
sqrt(9)


def AverageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

def sqrt2(x:Double) = FixedPoint(AverageDamp((y: Double) => x / y))(1) // que nO moco
sqrt2(9)

//--------------------------------------------------------

class Rational(x:Int, y:Int)
{
  require(y != 0, "denominator must be nonzero")

  def this(x:Int) = this(x, 1)  //non primary constructor

  private def gcd(a:Int, b:Int):Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
  def numer = x / g   // will be calculated everytime numer is used
  val denom = y / g // will be calculated in the definition and used the value instead

  def neg(): Rational =
  {
    new Rational(-numer, denom)
  }

  def add(rhs:Rational):Rational =
  {
    new Rational(numer * rhs.denom + rhs.numer * denom, denom * rhs.denom)
  }

  def sub(rhs:Rational):Rational = add(rhs.neg)

  override def toString = numer + "/" + denom

  def <(rhs:Rational) = numer * rhs.denom < rhs.numer * denom

  def max(rhs:Rational) = if (<(rhs)) rhs else this
}

var x = new Rational(1, 3)
var y = new Rational(5, 7)
var z = new Rational(3, 2)

x.neg
x.sub(y).sub(z)
x.add(y)

x.<(y) //x < y for infix
x.max(y)
x max y //infix operator syntax

//val strange = new Rational(1, 0) //will throw error (divide by 0)...guard with class requirement
//strange.add(strange)

val second_construred = new Rational(1)





