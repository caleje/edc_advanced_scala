import cats.data.Validated.Invalid

object Example {
  import cats._
  import cats.std.all._
  import cats.syntax.all._
  import cats.data.Validated
  import cats.data.Validated.Valid

  //Applicative[Option].pure(1)
  (Option(1) |@| Option(2)).tupled

  import cats.data.Xor
  type StringOrInt = String Xor Int
  val soi1: StringOrInt = Xor.right(123)
  val soi2: StringOrInt = Xor.left("oyvay")
  val soi3: StringOrInt = Xor.left("yipes")
  // soi1 |@| soi2

  for {
    i1 <- soi1
    i3 <- soi3
    i2 <- soi2
  } yield i1 + i2 + i3
//  val soi1 = Xor[String, Int].right(123)
//  val soi2 = Xor[Int].left("error on 123")

  /*val v1: Validated[String, Int] = Valid(123)
  val v2: Validated[String, Int] = Valid(246)
  for {
    vv1 <- v1
    vv2 <- v2
  } yield vv1 // + vv2*/


}