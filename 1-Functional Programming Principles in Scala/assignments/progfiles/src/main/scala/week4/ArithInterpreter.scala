package week4

class ArithInterpreter
{

}

    /*
trait Expr
{
    //classification
    def isNumber: Boolean
    def isSum: Boolean
    //accessors
    def numValue: Int
    def leftOp: Expr
    def rightOp: Expr
}

class Number(val numValue: Int) extends Expr
{
    override def isNumber: Boolean = true
    override def isSum: Boolean = false

    override def leftOp: Expr = throw new Error("NUmber.leftOP")
    override def rightOp: Expr = throw new Error("NUmber.rightOP")
}

class Sum(e1: Expr, e2:Expr) extends Expr
{
    override def isNumber: Boolean = false
    override def isSum: Boolean = true

    override def numValue: Int = throw new Error("Sum.numValue")
    override def leftOp: Expr = e1
    override def rightOp: Expr = e2
}

def eval(e: Expr): Int =
{
    if (e.isNumber) e.numValue
    else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
    else throw new Error("Unknown expression " + e)
}

*/

trait Expr
{
    def eval: Int = this match
    {
        case Number(n) => n
        case Sum(e1, e2) => e1.eval + e2.eval
        case Prod(e1, e2) => e1.eval * e2.eval
        case Var(x) => 0
    }

    def show: String = this match
    {
        case Number(n) => n.toString
        case Sum(e1, e2) => e1.show + "+" + e2.show
        case Prod(e1, e2) => e1.show + "*" + e2.show
        case Var(name) => name
    }
}

case class Number(n: Int) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

case class Prod(e1: Expr, e2: Expr) extends Expr

case class Var(name: String) extends Expr

