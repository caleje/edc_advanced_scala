
object CatsValidated {
  import cats.data.Validated
  import cats.data.Validated.{Invalid, Valid}

  trait Read[A] {
    def read(s: String): Option[A]
  }

  object Read {
    def apply[A](implicit A: Read[A]): Read[A] = A

    implicit val stringRead: Read[String] =
      new Read[String] { def read(s: String): Option[String] = Some(s) }

    implicit val intRead: Read[Int] =
      new Read[Int] {
        def read(s: String): Option[Int] =
          if (s.matches("-?[0-9]+")) Some(s.toInt)
          else None
      }
  }

  sealed abstract class ConfigError
  final case class MissingConfig(field: String) extends ConfigError
  final case class ParseError(field: String) extends ConfigError

  case class Config(map: Map[String, String]) {
    def parse[A : Read](key: String): Validated[ConfigError, A] =
      map.get(key) match {
        case None        => Invalid(MissingConfig(key))
        case Some(value) =>
          Read[A].read(value) match {
            case None    => Invalid(ParseError(key))
            case Some(a) => Valid(a)
          }
      }
  }

  import cats.Semigroup

  def parallelValidate[E : Semigroup, A, B, C](v1: Validated[E, A], v2: Validated[E, B])(f: (A, B) => C): Validated[E, C] =
    (v1, v2) match {
      case (Valid(a), Valid(b))       => Valid(f(a, b))
      case (Valid(_), i@Invalid(_))   => i
      case (i@Invalid(_), Valid(_))   => i
      case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
    }

  import cats.SemigroupK
  import cats.data.NonEmptyList
  import cats.std.list._

  case class ConnectionParams(url: String, port: Int)

  val config = Config(Map(("endpoint", "127.0.0.1"), ("port", "not an int")))

  implicit val nelSemigroup: Semigroup[NonEmptyList[ConfigError]] =
    SemigroupK[NonEmptyList].algebra[ConfigError]

  implicit val readString: Read[String] = Read.stringRead
  implicit val readInt: Read[Int] = Read.intRead

  val v1 = parallelValidate(config.parse[String]("url").toValidatedNel,
    config.parse[Int]("port").toValidatedNel)(ConnectionParams.apply)

  val v2 = parallelValidate(config.parse[String]("endpoint").toValidatedNel,
    config.parse[Int]("port").toValidatedNel)(ConnectionParams.apply)

  import cats.Applicative

  implicit def validatedApplicative[E : Semigroup]: Applicative[Validated[E, ?]] =
    new Applicative[Validated[E, ?]] {
      def ap[A, B](f: Validated[E, A => B])(fa: Validated[E, A]): Validated[E, B] =
        (fa, f) match {
          case (Valid(a), Valid(fab)) => Valid(fab(a))
          case (i@Invalid(_), Valid(_)) => i
          case (Valid(_), i@Invalid(_)) => i
          case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
        }

      def pure[A](x: A): Validated[E, A] = Validated.valid(x)
    }

  import cats.Apply
  import cats.data.ValidatedNel

  val config1 = Config(Map(("name", "cat"), ("age", "not a number"), ("houseNumber", "1234"), ("lane", "feline street")))

  case class Address(houseNumber: Int, street: String)
  case class Person(name: String, age: Int, address: Address)


  val personFromConfig: ValidatedNel[ConfigError, Person] =
       Apply[ValidatedNel[ConfigError, ?]].map4(config.parse[String]("name").toValidatedNel,
                                                config.parse[Int]("age").toValidatedNel,
                                              config.parse[Int]("house_number").toValidatedNel,
                                              config.parse[String]("street").toValidatedNel) {
         case (name, age, houseNumber, street) => Person(name, age, Address(houseNumber, street))
         }

  import cats.Monad

  implicit def validatedMonad[E]: Monad[Validated[E, ?]] =
    new Monad[Validated[E, ?]] {
      def flatMap[A, B](fa: Validated[E, A])(f: (A) => Validated[E, B]): Validated[E, B] =
        fa match {
          case i@Invalid(_) => i
          case Valid(a) => f(a)
        }

      def pure[A](x: A): Validated[E, A] = Valid(x)
    }

  val v = validatedMonad.tuple2(Validated.invalidNel[String, Int]("oops"), Validated.invalidNel[String, Double]("uh oh"))

  val houseNumber = config1.parse[Int]("house_number").andThen{ n =>
        if (n >= 0) Validated.valid(n)
        else Validated.invalid(ParseError("house_number"))
     }

  import cats.data.Xor

  def positive(field: String, i: Int): ConfigError Xor Int = {
    if (i >= 0) Xor.right(i)
    else Xor.left(ParseError(field))
  }
}