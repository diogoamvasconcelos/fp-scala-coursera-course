package week4

abstract class Nat
{
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat = new Succ(this)
    def + (that: Nat): Nat
    def - (that: Nat): Nat
}

object Zero extends Nat
{
    override def isZero: Boolean = true

    override def predecessor: Nat = throw new NoSuchElementException("number can't be negative")

    override def +(that: Nat): Nat = that
    override def -(that: Nat): Nat = if (that.isZero) this else throw new NoSuchElementException("number can't be negative")
}

class Succ(n: Nat) extends  Nat
{
    override def isZero: Boolean = false

    override def predecessor: Nat = n

    /*
    override def +(that: Nat): Nat =
    {
        def iter_+(curr: Nat, acc: Nat): Nat =
        {
            if (curr.isZero) acc
            else iter_+(n, new Succ(acc))
        }

        iter_+(this, that)
    }
    */
    override def +(that: Nat): Nat = new Succ(n + that)  //same as on top, but better, as n is the going down and Succ will go the accumulator
    override def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor // predecessor will make the "acc" move down
}
