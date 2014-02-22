package vindinium 

package object wolfie {

  type Path = List[Pos]

  implicit final class debugKCombinatorAny[A](a: A) {
    def pp: A = { println(s"*DEBUG* $a"); a }
  }

  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global
  implicit final class debugKCombinatorFutureAny[A](fua: Future[A]) {
    def thenPp: Future[A] = {
      fua onComplete { case result ⇒ result.pp }
      fua
    }
  }

  import vindinium.wolfie.Dir.Dir

  implicit final class OptionDirZero(od: Option[Dir]) {
    def unary_~ = od getOrElse Dir.Stay
  }

  // implicit final class BotPimpedOption
}
