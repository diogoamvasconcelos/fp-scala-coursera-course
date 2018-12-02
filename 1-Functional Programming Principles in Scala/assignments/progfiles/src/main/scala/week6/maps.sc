val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)



class Poly(val terms: Map[Int, Double])
{
    def + (other: Poly) =
    {
        //terms ++ other.terms  //returns Map of the concatenation, but we want a Poly object
        new Poly(terms ++ (other.terms map adjust))
    }

    def adjust(term: (Int, Double)): (Int, Double) = //break down the problem of adding values with the same key
    {
        val (exp, coeff) = term
        terms.get(exp) match
        {
            case Some(this_coeff) => exp -> (coeff + this_coeff)
            case None => exp -> coeff
        }
    }

    /*
    override def toString =
        (for ((exp, coef) <- terms) yield coef + "x^" + exp) mkString(" + ")  //prints unsorted
    */

    override def toString =
        (for ((exp, coef) <- terms.toList.sorted.reverse) yield coef + "x^" + exp) mkString(" + ")  //prints sorted
}

val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))

val p3 = p1+p2