package freemonads

import cats._
import cats.free.Free

object SimpleProgram extends App {
  sealed trait Expr[A]

  case class Val[A](v: A) extends Expr[A]
  case class Add[A](e1: A, e2: A) extends Expr[A]
  case class Sub[A](e1: A, e2: A) extends Expr[A]

  def value[A](v: A) = Free.liftF[Expr, A](Val(v))
  def add[A](e1: A, e2: A) = Free.liftF[Expr, A](Add(e1, e2))
  def subtract[A](e1: A, e2: A) = Free.liftF[Expr, A](Sub(e1, e2))

  def renderInterpreter = new (Expr ~> Id) {
    override def apply[A](fa: Expr[A]): Id[A] = fa match {
      case Val(v) => v.asInstanceOf[A]
      case Add(e1, e2) => s"($e1 + $e2)".asInstanceOf[A]
      case Sub(e1, e2) => s"($e1 - $e2)".asInstanceOf[A]
    }
  }

  def evalInterpreter = new (Expr ~> Id) {
    override def apply[A](fa: Expr[A]) = fa match {
      case Val(v) => v.asInstanceOf[A]
      case Add(e1, e2) => (e1.asInstanceOf[Int] + e2.asInstanceOf[Int]).asInstanceOf[A]
      case Sub(e1, e2) => (e1.asInstanceOf[Int] - e2.asInstanceOf[Int]).asInstanceOf[A]
    }
  }

  def program[A] =
    for {
      a <- value(1.asInstanceOf[A])
      b <- value(2.asInstanceOf[A])
      c <- add(a, b)
      d <- subtract(c, b)
    } yield d

  println(program.foldMap(renderInterpreter))
  println(program.foldMap(evalInterpreter))

}
