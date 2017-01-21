package freemonads

import cats._
import cats.free.{:<:, Free, Inject}

import scala.collection.mutable
import cats.data.Coproduct
import scala.io.StdIn._
import scala.language.higherKinds

object DSL {
  implicit def lift[F[_], G[_], A](fa: F[A])(implicit I: F :<: G): Free[G, A] =
    Free.inject[F, G](fa)(Inject[F, G])

  sealed trait Interact[A]

  object Interact {
    case class Ask(prompt: String) extends Interact[String]
    case class Tell(msg: String) extends Interact[Unit]
  }

  sealed trait Game[A]

  object Game {
    case class AddPlayer(id: Move, name: String) extends Game[Boolean]
    case class AddMove(move: String) extends Game[Boolean]
    case class PrintBoard() extends Game[Unit]
    case class IsGameOver() extends Game[Boolean]
    case class GetWinner() extends Game[Option[String]]
  }

  sealed trait ProgramState

  object ProgramState {
    case object Continue extends ProgramState
    case object End extends ProgramState
  }

  implicit class EvalMove(val o: Move) extends AnyVal {
    import Move._
    def eval: Char = {
      o match {
        case Empty => ' '
        case _ => o.toString.head
      }
    }
  }

  sealed trait Move

  object Move {
    case object Empty extends Move
    case object O extends Move
    case object X extends Move
  }

  type PRG[A] = Coproduct[Interact, Game, A]
}

object Interpreters {
  import DSL._
  import Interact._
  import Game._
  import Move._

  def gameInterpreter: (Game ~> Id) = new (Game ~> Id) {
    object Axis extends Enumeration {
      type Axis = Value
      val Row, Col, DiagL, DiagR = Value
    }

    val board: mutable.Map[(Int, Int), Move] = mutable.Map.empty
    val players: mutable.Map[Move, String] = mutable.Map.empty
    val numPlayers = 2
    val boardN = 3

    players.sizeHint(numPlayers)
    board.sizeHint(boardN * boardN)

    def nextMove(): Move = {
      board.values.foldLeft((0, 0)) {
        case ((x, o), Empty) => (x, o)
        case ((x, o), X) => (x + 1, o)
        case ((x, o), O) => (x, o + 1)
      } match {
        case (x, o) if x - o <= 0 => X
        case _ => O
      }
    }

    def reduceOpt(a: Option[Move], b: Option[Move]): Option[Move] = {
      (a, b) match {
        case (Some(aa), Some(bb)) => if (aa.toString == bb.toString) Some(aa.asInstanceOf[Move]) else None
        case _ => None
      }
    }

    def exposeOpt(a: Option[Move], b: Option[Move]): Option[Move] = {
      (a, b) match {
        case (Some(aa), _) => a
        case (_, Some(bb)) => b
        case _ => None
      }
    }
    def isBoardFull: Boolean = board.size == (boardN * boardN)

    def check(v: Option[Int], axis: Axis.Value): Option[Move] = {
      (0 until boardN).map(i =>
        board.getOrElse(
          if (axis == Axis.Row) (v.get, i)
          else if (axis == Axis.Col) (i, v.get)
          else if (axis == Axis.DiagR) (boardN - i - 1, i)
          else (i, i), None)
      )
        .map(a => a match {
          case None => None
          case _ => Some(a.asInstanceOf[Move])
        })
        .reduce((a, b) =>
          reduceOpt(a, b))
    }

    def getWinner: Option[String] = {
      (0 until boardN).map(i =>
        Seq(check(Some(i), Axis.Row), check(Some(i), Axis.Col), check(None, Axis.DiagL), check(None, Axis.DiagR)).reduce((a, b) => exposeOpt(a, b))
      ).reduce((a, b) => exposeOpt(a, b)) match {
        case Some(m) => Some(players(m))
        case None => None
      }
    }

    def isGameOver: Boolean = {
      isBoardFull || getWinner.isDefined
    }
    
    override def apply[A](fa: Game[A]): Id[A] = fa match {
      case AddPlayer(id, name) =>
        if (players.size <= 2) {
          players += id -> name
          true.asInstanceOf[A]
        }
        else {
          false.asInstanceOf[A]
        }

      case AddMove(move) =>
        val m: Array[String] = move.split(" ")
        if (m.length == 2 && m.map(v => v.length == 1 && v.head.isDigit && v.toInt >= 0 && v.toInt < boardN).reduce((a ,b) => a && b)
            && !(board contains (m(0).toInt, m(1).toInt))) {
          board += (m(0).toInt, m(1).toInt) -> nextMove()
          true.asInstanceOf[A]
        }
        else {
          false.asInstanceOf[A]
        }

      case PrintBoard() =>
        println("_ " * ((boardN * 2) - 1 ))
        for (i <- 0 until boardN) {
          val line = new mutable.StringBuilder()
          line += '|'
          for (j <- 0 until boardN) {
            line += board.getOrElse((i, j), Empty).eval
            line ++= " |"
          }
          println(line.toString())
          println("_ " * ((boardN * 2) - 1 ))
        }
        {}.asInstanceOf[A]

      case IsGameOver() =>
        isGameOver.asInstanceOf[A]

      case GetWinner() =>
        getWinner.asInstanceOf[A]
    }
  }
  def interactInterpreter: (Interact ~> Id) = new (Interact ~> Id) {
    override def apply[A](fa: Interact[A]): Id[A] = fa match {
      case Ask(prompt) =>
        println(prompt)
        readLine().asInstanceOf[A]

      case Tell(msg) =>
        println(msg).asInstanceOf[A]
    }
  }
}

object Program {
  import DSL._
  import Interact._
  import Game._
  import ProgramState._
  import Move._

  def userProgram[F[_]](implicit I: Interact :<: F, G: Game :<: F): Free[F, ProgramState] = {
    for {
      name1 <- Ask("What is Player 1's name?")
      _ <- AddPlayer(X, name1)
      name2 <- Ask("What is Player 2's name?")
      _ <- AddPlayer(O, name2)
    } yield Continue
  }

  def moveProgram[F[_]](implicit I: Interact :<: F, G: Game :<: F): Free[F, Unit] = {
    for {
      move <- Ask("Enter your move (e.g. 1 2):")
      s <- AddMove(move)
      n <- if (s) {
        resolveProgram
      } else {
        invalidProgram
      }
      _ <- n match {
        case End => Free.pure[F, String]("Done")
        case Continue => moveProgram
      }
    } yield ()
  }

  def resolveProgram[F[_]](implicit I: Interact :<: F, G: Game :<: F): Free[F, ProgramState] = {
    for {
      _ <- PrintBoard()
      res <- IsGameOver()
      s <- if (res) {
        winnerProgram
      }
      else {
        nextTurnProgram
      }
    } yield s
  }

  def winnerProgram[F[_]](implicit I: Interact :<: F, G: Game :<: F): Free[F, ProgramState] = {
    for {
      a <- GetWinner()
      _ <- a match {
        case Some(winner) => Tell(s"Game Over, $winner has won!")
        case None => Tell(s"Game Over, no one has won :(")
      }
    } yield End
  }

  def nextTurnProgram[F[_]](implicit I: Interact :<: F, G: Game :<: F): Free[F, ProgramState] = {
    for {
      _ <- Tell("Next turn")
    } yield Continue
  }

  def invalidProgram[F[_]](implicit I: Interact :<: F, G: Game :<: F): Free[F, ProgramState] = {
    for {
      _ <- Tell("Invalid entry, try again")
    } yield Continue
  }

  def program[F[_]](implicit I: Interact :<: F, G: Game :<: F): Free[F, Unit] = {
    for {
      a <- userProgram
      b <- moveProgram
    } yield ()
  }
}

object TicTacProgram extends App {
  import Program._
  import Interpreters._
  import DSL._

  val interpreter: PRG ~> Id = interactInterpreter or gameInterpreter

  val prog = program[PRG]
  prog.foldMap(interpreter)
}
