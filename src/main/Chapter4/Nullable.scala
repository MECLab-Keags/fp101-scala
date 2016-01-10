//package fp101.errorhandling
/**
  * Chapter 4 - Handling errors without exceptions
  */
sealed trait Nullable[+A] {
    /** Exercise 4.1 - Implement helpful functions */

    def map[B](f : A => B) : Nullable[B] = this match {
        case Value(a) => Value(f(a))
        case _ => Empty
    }

    def flatMap[B](f : A => Nullable[B]) : Nullable[B] =
        map(f) getOrElse Empty

    def getOrElse[B >: A](default: => B) : B = this match {
        case Value(a) => a
        case Empty => default
    }

    def orElse[B >: A](ob: => Nullable[B]) : Nullable[B] =
        this map(Value(_)) getOrElse(ob)

    def filter(f : A => Boolean) : Nullable[A] = this match {
        case Value(a) if f(a) => this
        case Empty => Empty
     }
}
case class Value[+A](value: A) extends Nullable[A]
case object Empty extends Nullable[Nothing]