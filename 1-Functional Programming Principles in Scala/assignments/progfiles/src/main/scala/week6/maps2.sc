class Poly(val terms0: Map[Int, Double])
{
    def this(bindings: (Int, Double)*) = this(bindings.toMap) //secondary construtor where we dont have to specify the Map
    val terms = terms0 withDefaultValue(0.0)
    def + (other: Poly) =
    {
        new Poly(terms ++ (other.terms map adjust))
    }

    def adjust(term: (Int, Double)): (Int, Double) = //break down the problem of adding values with the same key
    {
        val (exp, coeff) = term
        exp -> (coeff + terms(exp)) //simplified using the default values (vs the maps.sc)
    }

    def ++ (other: Poly) =  //diff implementation of '+'
    {
        new Poly((other.terms foldLeft terms)(addTerm))  //more efficient than '+'
    }

    def addTerm(terms: Map[Int, Double], term: (Int, Double)) =
    {
        terms + (term._1 -> (term._2 + terms(term._1))) //same
        //terms updated (term._1, term._2 + terms(term._1)) //same
    }


    override def toString =
        (for ((exp, coef) <- terms.toList.sorted.reverse) yield coef + "x^" + exp) mkString " + "  //prints sorted
}

val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))

val p3 = p1+p2

p1.terms(7) //returns default value

val p4 = new Poly(2 -> 2.0, 3 -> 4.0, 5 -> 6.2)

val p5 = p1 ++ p2

