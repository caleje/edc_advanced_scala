
object Checks {
  import me.krobinson.monads.Suspend
  import me.krobinson.monads.Free

  sealed trait MyOps[A]
  case class AddOne[A](a: A) extends MyOps[A]
  case class Combine[A](a1: A, a2: A) extends MyOps[A]
  case class Filter[A](a: A, f: A => Boolean) extends MyOps[A]

  object MyOps {
    def newAddOne[A](a: A): Free[MyOps, A] = Suspend(AddOne(a))
    def newCombine[A](a1: A, a2: A): Free[MyOps, A] = Suspend(Combine(a1, a2))
    def newFilter[A](a: A, f: A => Boolean): Free[MyOps, A] = Suspend(Filter(a, f))
  }

  val x = for {
    a <- MyOps.newAddOne(1)
    b <- MyOps.newAddOne(2)
  } yield (a, b)
}