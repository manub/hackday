package scalaworld

import cats._
import cats.free._
import cats.data.State

sealed trait WangBMachineOp[A]

package object scalaworld {
  type Symbol = String
  case object Failure
  type Failure = Failure.type
}

import scalaworld._

sealed trait Direction
case object Left extends Direction
case object Right extends Direction

case class Move(direction: Direction) extends WangBMachineOp[Unit]
case class Write(symbol: Symbol) extends WangBMachineOp[Unit]
case object Read extends WangBMachineOp[Symbol]
//case object Accept extends WangBMachineOp[Symbol]
case object Reject extends WangBMachineOp[Failure]

object WangBMachine {

  type WangBMachine[A] = Free[WangBMachineOp, A]

  def move(d: Direction) = Free.liftF(Move(d))
  def write(s: Symbol) = Free.liftF(Write(s))
  def read() = Free.liftF(Read)
//  def accept() = Free.liftF(Accept)
  def reject() = Free.liftF(Reject)

  def program1(): WangBMachine[Symbol] = for {
    symbol <- read()
    _ <- move(Right)
    _ <- write(symbol)
    lastSymbol <- read()
  } yield lastSymbol

  case class TuringTape(tape: Vector[Symbol], pointer: Int)
  type TuringTapeState[A] = State[TuringTape, A]


  def turingTape = new (WangBMachineOp ~> TuringTapeState) {

    override def apply[A](fa: WangBMachineOp[A]): TuringTapeState[A] = {
      fa match {
        case Reject => State.pure(Failure)
        case Read => State { turingTape =>
          (turingTape, turingTape.tape(turingTape.pointer))
        }
        case Write(s) => State { turingTape =>
          (turingTape.copy(tape = turingTape.tape.updated(turingTape.pointer, s)), ())
        }
        case Move(Left) => State { turingTape =>
          (turingTape.copy(pointer = turingTape.pointer - 1), ())
        }
        case Move(Right) => State { turingTape =>
          (turingTape.copy(pointer = turingTape.pointer + 1), ())
        }
      }
    }
  }

}

object Runner extends App {

  import WangBMachine._

  private val stateMonad = program1().foldMap(turingTape)

  println(stateMonad.runS(TuringTape(Vector("42", "0"), 0)).value)



}






