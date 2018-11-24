import week3._

abstract class IntSet
{
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet //singleton
{
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def contains(x: Int): Boolean = false

  override def union(other: IntSet): IntSet = other

  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet
{
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  override def union(other: IntSet): IntSet =
  {
    ((left union right) union other) incl elem
  }

  override def toString = "{" + left.toString + "-" +  elem + "-" + right.toString + "}"
}


val t1 = new NonEmpty(3, Empty, Empty)
var t2 = t1 incl 4

//--------------------------------------------------------------


/*
def nth[T](n: Int, list: List[T]): T =
{
    if (list.isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) list.head
    else nth(n-1, list.tail)
}


val list1 = new Cons(1, new Cons(2, new Cons(3, new Nil)))

nth(2, list1)
nth(-1, list1)
*/