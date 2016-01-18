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

object Nullable {
    def lift[A,B](f: A => B) : A => Nullable[B] =  Value(_) map f

    def map2[A,B,C](a: Nullable[A], b: Nullable[B])(f: (A, B) => C): Nullable[C] =
        a flatMap (aa => b map (bb => f(aa, bb)))

    def sequence[A](xs : List[Nullable[A]]) : Nullable[List[A]] = xs match {
        case Nil => Value(Nil)
        case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }
}

object Chapter4 {
    def mean(xs : Seq[Double]) : Nullable[Double] = {
        if (xs.isEmpty) Empty
        else Value(xs.sum / xs.length)
    }

    def variance(xs : Seq[Double]) : Nullable[Double] = {
        mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    }

    val absO : Double => Nullable[Double] = Nullable.lift(math.abs)
}