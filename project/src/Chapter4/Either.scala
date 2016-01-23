trait Either[+E,+A]{
  def map[B](f: A => B): Either[E,B]
  def flatMap[EE >: E,B](f: A => Either[EE,B]): Either[EE,B]
}