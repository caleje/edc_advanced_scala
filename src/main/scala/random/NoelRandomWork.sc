object NoelRandomWork {
  import scala.annotation.tailrec

  final case class Random[A](events: List[(A, Double)]) extends Product with Serializable {
    def run(rng: scala.util.Random = scala.util.Random): A = {
      val weight = rng.nextDouble()

      @tailrec
      def pick(total: Double, events: List[(A, Double)]): A =
        events match {
          case (a, p) :: rest =>
            if(total < weight && weight < (total + p))
              a
            else
              pick(total + p, rest)
          case Nil =>
            throw new Exception("Could not run--ran out of events!")
        }
      pick(0.0, this.compact.normalize.events)
    }

    def flatMap[B](f: A => Random[B]): Random[B] = {
      Random(
        for {
          (a, p1) <- this.events
          (b, p2) <- f(a).events
        } yield b -> (p1 * p2)
      )
    }

    def product[B](other: Random[B]): Random[(A,B)] =
      Random(
        for {
          (a, p1) <- this.events
            (b, p2) <- other.events
        } yield (a,b) -> (p1 * p2)
      )

    def normalize: Random[A] = {
      val totalWeight = (events map { case (a, p) => p }).sum
      Random(events map { case (a,p) => a -> (p / totalWeight) })
    }

    def compact: Random[A] = {
      val distinct = (events map { case (a, p) => a }).distinct
      def prob(a: A): Double =
        (events filter { case (x, p) => x == a } map { case (a, p) => p }).sum

      Random(distinct map { a => a -> prob(a) })
    }
  }
  object Random {
    def always[A](a: A): Random[A] =
      Random(List(a -> 1.0))

    def oneOf[A](as: A*): Random[A] = {
      val size = as.size
      Random(as.map { a => a -> (1.0 / size) }.toList)
    }

    def discrete[A](events: (A, Double)*): Random[A] =
      Random(events.toList)
  }
  sealed trait Cat extends Product with Serializable
  final case object OneCat extends Cat
  final case object TwoCat extends Cat
  final case object ThreeCat extends Cat

  sealed trait Enticement extends Product with Serializable
  final case object MilkShake extends Enticement
  final case object Fish extends Enticement
  final case object Nada extends Enticement

  object CatModel {
    def distribution =
      Random.discrete(MilkShake -> 0.6, Fish -> 0.1, Nada -> 0.3).flatMap[(Enticement, Cat)] {
        case MilkShake =>
          Random.discrete((MilkShake, OneCat) -> 0.1, (MilkShake, TwoCat) -> 0.2, (MilkShake, ThreeCat )-> 0.7)

        case Fish =>
          Random.discrete((Fish, OneCat) -> 0.2, (Fish, TwoCat )-> 0.4, (Fish, ThreeCat )-> 0.4)

        case Nada =>
          Random.discrete((Nada, OneCat) -> 0.6, (Nada, TwoCat )-> 0.3, (Nada, ThreeCat )-> 0.1)
      }
  }
}