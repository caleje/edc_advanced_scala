object RandomWork {

  import cats.Monad
  import cats.Applicative

  final case class Random[A](events: List[(A, Double)]) extends Product with Serializable with Monad[Random] {
    /*def run: A = ???

    def flatMap[B](f: A => Random[B]): Random[B] = {

      Random(
        for {
          (a, d) <- events
          (b, dd)  <- f(a).events
        } yield b -> (d * dd)
      )

      /*val listBd = events.flatMap { case (a: A, d: Double) => {
        val bds = f(a).events.map { case (bb: B, dd: Double) => (bb, d * dd) }
        bds
      }
      }
      Random(listBd)*/
    }

    def product[B](other: Random[B]): Random[(A, B)] = {
      Random(
        for {
          (a, d) <- events
          (b, dd) <- other.events
        } yield ((a, b), d*dd)
      )
    }*/

    override def pure[A](x: A): Random[A] = Random[A](List((x, 1.0)))

    override def flatMap[A, B](fa: Random[A])(f: (A) => Random[B]): Random[B] = {
      Random(
        for {
          (a, d) <- fa.events
          (b, dd)  <- f(a).events
        } yield b -> (d * dd)
      )
    }
  }

  object Random {
  }

  sealed trait Cat extends Product with Serializable
  final case object OneCat extends Cat
  final case object TwoCats extends Cat
  final case object ThreeCats extends Cat
  sealed trait Enticement extends Product with Serializable
  final case object Fish extends Enticement
  final case object Milk extends Enticement
  final case object Nada extends Enticement
  val dE = Random(List((Fish, 0.6), (Milk, 0.3), (Nada, 0.1)))
  val dC = Random(List((OneCat, 0.5), (TwoCats, 0.4), (ThreeCats, 0.1)))

  Monad[Random].flatMap[Enticement, Cat](dE) { (e: Enticement) =>
    e match {
      case Fish => Random(List((OneCat, 0.4), (TwoCats, 0.3), (ThreeCats, 0.1)))
      case Milk => Random(List((OneCat, 0.4), (TwoCats, 0.3), (ThreeCats, 0.1)))
      case Nada => Random(List((OneCat, 0.4), (TwoCats, 0.3), (ThreeCats, 0.1)))
    }
  }
  //dE.product(dC)
}