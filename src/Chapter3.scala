/**
  * Chapter 3: Functional Data Structures
  */
package fp101.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def sum(xs: List[Int]): Int = xs match {
        case Nil => 0
        case Cons(y, ys) => y + sum(ys)
    }

    def product(xs: List[Double]): Double = xs match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(y, ys) => y * product(ys)
    }

    // A variadic function (a function that takes zero, one or more
    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
}
object Chapter3 {
    /** Exercise 3.1 - Pattern matching*/
    def e31(xs : List[Int]) : Int = {
        xs match {
            case Cons(x, Cons(2, Cons(4, _))) => x
            case Nil => 42
            case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
            case Cons(h, t) => h + List.sum(t)
            case _ => 101
        }
    }


    /** Exercise 3.2 - Implement a tail function
      *
      * Notes: If the function consists of just a single statement then it is good practice
      * to put it on the same line as the method signature, rather than introducing another level of nesting.
      * We could also throw an exception on the Nil case, but this is not the functional way.
      * So Nil is returned instead. */
    def e32[A](xs : List[A]) : List[A] = xs match {
        case Nil => Nil
        case Cons(_, t) => t
    }

    /** Exercise 3.3 - Replace the head of the given list.
      *
      * Notes: The Nil case in this function, however, makes sense to return a new list with
      * just the head set and no tail rather than returning Nil */
    def e33[A](h : A, xs : List[A]) : List[A] = xs match {
        case Nil => Cons(h, Nil)
        case Cons(_, t) => Cons(h, t)
    }

    /** Exercise 3.4 - Generalize exercise 3.2 to drop n number of elements and return the tail. */
    def e34[A](n : Int, xs : List[A]) : List[A] = xs match {
        case Nil => Nil
        case Cons(_, t) => {
            @annotation.tailrec
            def go(x : Int, ys : List[A]) : List[A] =
                if(x == n) t
                else go(x + 1, t)
            go(1, t)
        }
    }
}
