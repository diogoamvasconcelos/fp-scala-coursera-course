val word = "hello"

word.groupBy((c: Char) => c).toList map (c => (c._1, c._2.length))

val a = "abc"
val b = List("abc")