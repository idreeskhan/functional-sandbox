package freemonads

import cats._
import cats.free.Free

object CalculatorProgram extends App {
  sealed trait Expr[A]

  case class Val[A](v: A) extends Expr[A]
  case class Add[A](e1: A, e2: A) extends Expr[A]
  case class Sub[A](e1: A, e2: A) extends Expr[A]
  case class Mul[A](e1: A, e2: A) extends Expr[A]
  case class Div[A](e1: A, e2: A) extends Expr[A]


  def value[A](v: A) = Free.liftF[Expr, A](Val(v))
  def add[A](e1: A, e2: A) = Free.liftF[Expr, A](Add(e1, e2))
  def subtract[A](e1: A, e2: A) = Free.liftF[Expr, A](Sub(e1, e2))
  def multiply[A](e1: A, e2: A) = Free.liftF[Expr, A](Mul(e1, e2))
  def divide[A](e1: A, e2: A) = Free.liftF[Expr, A](Div(e1, e2))


  def renderInterpreter = new (Expr ~> Id) {
    override def apply[A](fa: Expr[A]): Id[A] = fa match {
      case Val(v) => v.asInstanceOf[A]
      case Add(e1, e2) => s"($e1 + $e2)".asInstanceOf[A]
      case Sub(e1, e2) => s"($e1 - $e2)".asInstanceOf[A]
      case Mul(e1, e2) => s"($e1 * $e2)".asInstanceOf[A]
      case Div(e1, e2) => s"($e1 / $e2)".asInstanceOf[A]
    }
  }

  def evalInterpreter = new (Expr ~> Id) {
    override def apply[A](fa: Expr[A]) = fa match {
      case Val(v) => v.asInstanceOf[A]
      case Add(e1, e2) => (e1.asInstanceOf[Double] + e2.asInstanceOf[Double]).asInstanceOf[A]
      case Sub(e1, e2) => (e1.asInstanceOf[Double] - e2.asInstanceOf[Double]).asInstanceOf[A]
      case Mul(e1, e2) => (e1.asInstanceOf[Double] * e2.asInstanceOf[Double]).asInstanceOf[A]
      case Div(e1, e2) => (e1.asInstanceOf[Double] / e2.asInstanceOf[Double]).asInstanceOf[A]
    }
  }

  def program[A] =
    for {
      a <- value(1.0.asInstanceOf[A])
      b <- value(2.0.asInstanceOf[A])
      c <- add(b, b)
      d <- subtract(c, a)
      e <- multiply(d, b)
      f <- divide(e, c)
    } yield f

  println(program.foldMap(renderInterpreter))
  println(program.foldMap(evalInterpreter))

}
