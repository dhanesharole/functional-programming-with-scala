sealed trait Stream[+A] {
  def headOption : Option[A] = {
    this match {
      case Empty => None
      case Cons(hd, td) => Some(hd())
    }
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](hd: () => A, td: () => Stream[A]) extends Stream[A]


object Stream {

  def cons[A](hd: => A, td: => Stream[A]): Cons[A] = {
    lazy val head = { println("head"); hd }
    lazy val tail = { println("tail"); td }
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](ele: A*): Stream[A] = {
    if (ele.isEmpty)
      empty
    else
      cons(ele.head, apply(ele.tail: _*))
  }
}


object StreamDemo extends App {

  val stream: Stream[Int] = Stream(1, 2, 3, 4)
  println(stream)
  println(stream.headOption)
}

