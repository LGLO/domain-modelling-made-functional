package ordertaking

import zio.IO
import scala.annotation.tailrec

sealed trait Result[+E, +T] {

  def flatMap[E1 >: E, T1](f: T => Result[E1, T1]): Result[E1, T1] = this match {
    case Ok(b) => f(b)
    case _     => this.asInstanceOf[Result[E1, T1]]
  }

  def map[T1](f: T => T1): Result[E, T1] = this match {
    case Ok(b) => Ok(f(b))
    case _     => this.asInstanceOf[Result[E, T1]]
  }

  def mapError[E1](f: E => E1): Result[E1, T] = this match {
    case Error(e) => Error(f(e))
    case _        => this.asInstanceOf[Result[E1, T]]
  }

  def toAsyncResult: IO[E, T] = this match {
    case Ok(value)    => IO.succeed(value)
    case Error(error) => IO.fail(error)
  }
}

object Result {

  def sequence[E, T](rs: Iterable[Result[E, T]]): Result[E, List[T]] = {
    @tailrec
    def go(acc: List[T], elems: List[Result[E, T]]): Result[E, List[T]] =
      elems.headOption match {
        case None               => Ok(acc)
        case Some(Error(error)) => Error(error)
        case Some(Ok(value))    => go(value :: acc, elems.tail)
      }
    go(Nil, rs.toList).map(_.reverse)
  }
}

case class Ok[+E, +T](value: T) extends Result[E, T]

case class Error[+E, +T](error: E) extends Result[E, T]
