import cats._
import cats.free._

object CatsFoldMapStackRecursionTest extends App {

  trait FTestApi[A]

  case class TB(i: Int) extends FTestApi[Int]

  type FTest[A] = Free[FTestApi, A]

  def tb(i: Int): FTest[Int] = Free.liftF(TB(i))

  def a(i: Int): FTest[Int] = for {
    j <- tb(i)
    z <- if (j < 10000000) a(j) else Free.pure[FTestApi, Int](j)
  } yield z

  def runner: FTestApi ~> Id = new (FTestApi ~> Id) {
    def apply[A](fa: FTestApi[A]): Id[A] = fa match {
      case TB(i) => i + 1 // TB is not generic - we know it takes an integer
    }
  }

  println(a(0).foldMap(runner))
}
