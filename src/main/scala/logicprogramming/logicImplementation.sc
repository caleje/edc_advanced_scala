
object logicImplementation {

  import cats.std.option._
  import cats.syntax.cartesian._

  sealed abstract class Logic[A] extends Product with Serializable {
    def and(that: Logic[A]): Logic[A] =
      Logic.And(this, that)

    def or(that: Logic[A]): Logic[A] =
      Logic.Or(this, that)

    def filter(f: A => Boolean): Logic[A] =
      Logic.Filter(this, f)

    def run: Option[A] =
      this match {
        case Logic.Pure(d) => Some(d.head)
        case Logic.Fail() => None
        case Logic.Filter(l, p) => ???
        case Logic.And(l, r) => (l.run |@| r.run).map { (a1, a2) => a1 }
      }


  }

  object Logic {
    final case class And[A](left: Logic[A], right: Logic[A]) extends Logic[A]
    final case class Or[A](left: Logic[A], right: Logic[A]) extends Logic[A]
    final case class Fail[A]() extends Logic[A]
    final case class Pure[A](domain: Stream[A]) extends Logic[A]
    final case class Filter[A](logic: Logic[A], predicate: A => Boolean) extends Logic[A]

    def fail[A]: Logic[A] =
      Fail()

    def pure[A](domain: Stream[A]): Logic[A] =
      Pure(domain)
  }
}