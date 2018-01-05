import scala.language.implicitConversions

trait Applicative[F[_]] {
  def unit[A] (f: A): F[A]

  def apply[A, B] (fa: F[A])(f: F[A => B]): F[B]
}

object Applicative {
  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def unit[A] (f: A) = Some ( f )

    override def apply[A, B] (fa: Option[A])(f: Option[A => B]): Option[B] = f match {
      case None => None
      case Some ( fn ) => fa.map ( fn )
    }

  }
}

class ApplicativeOps[F[_], A] (data: F[A]) {

  def <*>[B] (f: F[A => B])(implicit applicative: Applicative[F]): F[B] = {
    applicative.apply ( data )( f )
  }
}

object ApplicativeOps {

  implicit def toApplicativeOps[F[_], A] (v: F[A]): ApplicativeOps[F, A] = {
    new ApplicativeOps ( v )
  }
}

object ApplicativeDemo extends App {


  import Applicative.optionApplicative
  import ApplicativeOps.toApplicativeOps

  val api1 = Option.apply ( "api1" )
  val api2 = Option.apply ( "api2" )
  val api3 = Option.apply ( "api3" )

  val apiLengthCalculator = ((a: String, b: String, c: String) => a.length + b.length + c.length).curried

  // scalaz api allows doing something similar with following syntax
  // 3.some <*> { 9.some <*> {(_: Int) + (_: Int)}.curried.some }
  //  println(api1 <*> api2 <*> api3 <*> optionApplicative.unit(apiLengthCalculator))
  println (api1 <*> {api2 <*> {api3 <*> {Option ( apiLengthCalculator )}}})

}
