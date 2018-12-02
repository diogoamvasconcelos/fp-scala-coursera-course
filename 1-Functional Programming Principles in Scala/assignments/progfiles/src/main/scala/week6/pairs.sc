//generate all pairs of intergers (i, j) that 1 <= j < i <  n
val n = 7

val pairs = (1 until n) map (i =>
                (1 until i) map (j => (i, j)))

pairs.flatten

val pairs2 = (1 until n) flatMap (i =>
    (1 until i) map (j => (i, j)))

