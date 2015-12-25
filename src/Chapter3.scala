/**
  * Chapter 3: Functional Data Structures
  */
object Chapter3 {
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
}
