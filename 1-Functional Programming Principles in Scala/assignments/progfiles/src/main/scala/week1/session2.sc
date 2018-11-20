def factorial(x:Double):Double =
{
  if (x == 0) 1 else x * factorial(x-1)
}

factorial(4)
factorial(3)

def factorial2(x:Double):Double =
{
  def TailFactorial(x:Double, sum:Double):Double =
  {
    if (x == 0) sum else TailFactorial(x-1, sum * x)
  }

  TailFactorial(x, 1)
}

factorial2(0)
factorial2(1)
factorial2(4)
factorial2(3)