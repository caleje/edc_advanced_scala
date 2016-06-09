package aborted

import cats.{Applicative, Monoid}
import cats.syntax.semigroup._

object Validation {

  sealed abstract class Result[A, E] extends Product with Serializable {
    def value: A

    def and(that: Result[A, E])(implicit m: Monoid[E]): Result[A, E] =
      (this, that) match {
        case (Success(_), Success(_)) => Success(value)
        case (Success(_), Failure(_, f)) => Failure(value, f)
        case (Failure(_, f1), Success(_)) => Failure(value, f1)
        case (Failure(_, f1), Failure(_, f)) => Failure(value, f1 |+| f)
      }

    def or(that: Result[A, E])(implicit m: Monoid[E]): Result[A, E] =
      (this, that) match {
        case (Success(_), Success(_)) => Success(value)
        case (Success(_), Failure(_, f)) => Success(value)
        case (Failure(_, f1), Success(_)) => Success(value)
        case (Failure(_, f1), Failure(_, f)) => Failure(value, f1 |+| f)
      }

    def prod[B](that: Result[B, E])(implicit m: Monoid[E]): Result[(A, B), E] =
      (this, that) match {
        case (Success(_), Success(b)) => Success((value, b))
        case (Success(_), Failure(b, f)) => Failure((value, b), f)
        case (Failure(_, f1), Success(b)) => Failure((value, b), f1)
        case (Failure(_, f1), Failure(b, f)) => Failure((value, b), f1 |+| f)
      }
  }

  final case class Success[A, E](value: A) extends Result[A, E]

  final case class Failure[A, E](value: A, failure: E) extends Result[A, E]

  sealed abstract class Check[A, E] {
    def apply(a: A)(implicit m: Monoid[E]): Result[A, E] =
      this match {
        case And(c1, c2) => c1(a) and c2(a)
        case Or(c1, c2) => c1(a) or c2(a)
        case Pure(f) => f(a)
        //case Prod(c1, c2) => c1(a) prod
      }

    def and(that: Check[A, E]): Check[A, E] =
      And(this, that)

    def or(that: Check[A, E]): Check[A, E] =
      Or(this, that)

    // def prod[B](that: Check[B,E]): Check[A,E] =
    // Prod(this, that)
    //def map???
    //def flatMap???
  }

  final case class And[A, E](left: Check[A, E], right: Check[A, E]) extends Check[A, E]

  final case class Or[A, E](left: Check[A, E], right: Check[A, E]) extends Check[A, E]

  final case class Prod[A, B, E](left: Check[A, E], right: Check[B, E]) extends Check[(A, B), E]

  final case class Pure[A, E](f: A => Result[A, E]) extends Check[A, E]

}
