object classNotes {
  (5 to 10).toList.filter(_ % 4 == 0)
  // Domain is List
  // Constraint is A => Boolean
  def filter[A](in: List[A])(pred: A => Boolean): List[A] = {
    for {
      a <- in if pred(a)
    } yield a
  }
  def filter1[A](in: List[A], pred: A => Boolean): List[A] = {
    in.flatMap { a => if( pred(a)) List(a) else List.empty}
  }
  filter((5 to 10).toList) (x => x % 4 == 0)
  filter1((5 to 10).toList, (x: Int) => x % 4 == 0)
  import cats.Monad
  import cats.MonoidK
  trait Logic[F[_]] extends Monad[F] with MonoidK[F]{
    def fail[A]: F[A]
    def filter[A]()
  }
  val domain = (5 to 10).toList
  def join[A](x: List[A], y: List[A]): List[A] = {
    x ++ y
  }
  def joinMonoid[F[_], A](x: F[A], y: F[A])(implicit m: MonoidK[F]): F[A] = {
    m.combineK(x, y)
  }
  join(filter(domain)(x => x % 4 == 0), filter(domain)(x => x % 2 == 1))
  // import cats.std.ListInstances
  joinMonoid(domain, domain)
  Stream.from(0)



  /////////////////////
  import cats.MonoidK

}