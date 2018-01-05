trait Functor[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]

  def lift[A, B](fa: F[A])(f: A => B): F[B] = fmap(fa)(f)
}

object Functor {

  implicit val listAsFunctor: Functor[List] = new Functor[List] {
    override def fmap[A, B](fa: List[A])(f: A => B): List[B] = {
      /* A real implementation using actual underlying data structure
      used for type List or any type F for which functor is to be created.
      In this case, it's List
       */
      fa.map(f)

    }
  }
}

object FunctorDemo extends App {

  def map[F[_], A, B](l: F[A])(f: A => B)(implicit functor: Functor[F]): F[B] = functor.fmap(l)(f)

  import Functor.listAsFunctor

  /* Functor laws on list */

  private val list = List(1, 2, 3, 4)
  /* 1. identity */
  private val list1: List[Int] = map(list) { identity }
  assert(list1 == list)

  /* 2. associativity */
  val f1: Int => Double = (x) => x.toDouble
  val f2: Double => Float = (x) => x.toFloat

  assert(map(list)(f1.andThen(f2)) == map(map(list)(f1))(f2))

  val f3: Int => Int => Int => Int = (i) => (i1) => (i2) => i * i1 * i2

  map(list)(f3)
}
