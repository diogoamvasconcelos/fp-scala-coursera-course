package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))

  println(forall(singletonSet(1), (x: Int) => x > 0 ))
  println(forall((x: Int) => x < 99999, (x: Int) => x < 1001 ))

  println(exists((x: Int) => x < 99999, (x: Int) => x == 0 ))
  println(exists((x: Int) => x < 99999, (x: Int) => x == 1001 ))
  println(exists(singletonSet(1), (x: Int) => x == 1 ))
  println(exists(singletonSet(1), (x: Int) => x == 0 ))

  println(contains(map(singletonSet(1), (x: Int) => x * 10), 1))
  println(contains(map(singletonSet(1), (x: Int) => x * 10), 10))
}
