package week3

import java.util.NoSuchElementException

trait List[T]
{
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
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