/*
object ValidationsUsage {
  import aborted.Validation._

  def IsEven(x: Int): Result[Int, String] = {
    if(x % 2 == 0) Success(x) else Failure(x, s"${x+1} would have been better")
  }
  def IsPositive(x: Int): Result[Int, String] = {
    if(x > 0) Success(x) else Failure(x, s"$x <= 0")
  }
  def IsVerbose(s: String): Result[String, String] = {
    if(s.length > 5) Success(s) else Failure(s, s"'$s' is too short")
  }

  def IsEvenAndIsPositive = And(Pure(IsEven), Pure(IsPositive))
  def IsEvenOrIsPositive = Or(Pure(IsEven), Pure(IsPositive))
  def IsEvenProductIsVerbose = Prod(Pure(IsEven), Pure(IsVerbose))

  import cats.std.string._
  val l = List(2,3,0,-2,-3)
  val results = l.map(IsEven)
  val results2 = l.map(x => IsEvenOrIsPositive(x))
  val results3 = l.map(x => IsEvenAndIsPositive(x))

  IsEvenOrIsPositive(3)
  IsEvenAndIsPositive(3)
  IsEvenProductIsVerbose(3, "123456")

  // Was hoping to have GADT problem
}
*/
