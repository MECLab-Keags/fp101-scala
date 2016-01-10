/**
  * Chapter 4 - Handling errors without exceptions
  */
/**
sealed trait Nullable[+A] {
    /** Exercise 4.1 - Implement helpful functions */
    /**
    def map[B](f : A => B) : Option2[B] = this match {
        case Some2(a) => Some2(f(a))
        case _ => None2
    }

    def flatMap[B](f : A => Option2[B]) : Option2[B] =
        map(f) getOrElse None2
    */
    def getOrElse[B >: A](default: => B) : B = this match {
        case Empty => default
        case Value(a) => a
    }
    /**
    def orElse[B >: A](ob: => Option2[B]) : Option2[B] =
        this map(Some2(_)) getOrElse(ob)

    def filter(f : A => Boolean) : Option2[A] = this match {
        case Some2(a) if f(a) => this
        case None2 => None2
     }
      */
}
  */
sealed trait Nullable[+A] {
    def get[B >: A](default: => B) : B = this match {
        case Value(v) => v
        case _ => default
    }
}
case class Value[+A](value: A) extends Nullable[A]
case object Null extends Nullable[Nothing]


/**
def get[B >: A](default: => B) : B = this match{
        case Value(v) => v
        case Null => default
    }
  * */