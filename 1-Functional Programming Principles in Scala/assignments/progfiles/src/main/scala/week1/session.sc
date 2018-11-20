1+5



def sqrt(x:Double)  =
{
  def abs(x:Double) = if (x < 0) -x else x

  def SqrIter(guess:Double):Double =
    if (IsGoodEnough(guess)) guess
    else SqrIter(Improve(guess))


  def IsGoodEnough(guess: Double):Boolean =
    abs((guess * guess) - x) < (x * 0.001)

  def Improve(guess: Double) =
    (guess + x / guess) / 2

  SqrIter(1.0)
}

sqrt(2.0)
sqrt(4.0)
sqrt(1e-6)
sqrt(1e50)