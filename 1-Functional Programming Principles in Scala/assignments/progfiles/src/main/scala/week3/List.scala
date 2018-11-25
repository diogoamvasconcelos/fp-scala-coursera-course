package week3

import java.util.NoSuchElementException

trait List[+T] //making List covariant (+) as part of lecture 4.4
{
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
    def pretend[U >: T](elem: U): List[U] = new Cons(elem, this) //using a lower bound to solve the contravariance problem
}

class Cons[T](val head: T, val tail: List[T]) extends List[T]
{
    // head and tail already implemented using the 'val' in the parameter list
    override def isEmpty: Boolean = false
}

class Nil[T] extends List[T]
{
    override def isEmpty: Boolean = true

    override def head: Nothing = throw new NoSuchElementException("Nil.head") //Nothing is a subtype of anyother type so this is allowed!
    override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object List
{
    //create constructor' functions for the List as objects
    //List(1, 2) = List.apply(1, 2)
    //List = List.apply

    def apply[T](x1: T, x2: T) : List[T] = new Cons(x1, new Cons(x2, new Nil))
    def apply[T] = new Nil[]
}


//Nill as an Object...(cant have a type parameter (generic), as there is onlu one instance of it now
// we can use Nothing (as it is the subtype of every type) if we make T covariant!, thus making List[T] covariant too
object Nil2 extends List[Nothing]
{
    override def isEmpty: Boolean = true

    override def head: Nothing = throw new NoSuchElementException("Nil.head") //Nothing is a subtype of anyother type so this is allowed!
    override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object test
{
    val x : List[String] = Nil2
}
