package me.martinmorek.part4typeclasses

import cats.{Applicative, Monad}
import cats.data.Validated

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object HandlingErrors {

  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    def raiseError[A](e: E): M[A]
    def handleErrorWith[A](ma: M[A])(func: E => M[A]): M[A]
    def handleError[A](ma: M[A])(func: E => A): M[A] = handleErrorWith(ma)(e => pure(func(e)))
  }

  trait MyMonadError[M[_], E] extends Monad[M] {
    def ensure[A](ma: M[A])(e: E)(predicate: A => Boolean): M[A]
  }

  import cats.MonadError
  import cats.instances.either._
  type ErrorOr[A] = Either[String, A]
  val monadErrorEither = MonadError[ErrorOr, String]
  val success: Either[String, Int] = monadErrorEither.pure(334)
  val failure: ErrorOr[Int] = monadErrorEither.raiseError[Int]("something wrong")
  val handleError: ErrorOr[Int] = monadErrorEither.handleError(failure){
    case "Error" => 23
    case _ => 42
  }

  val handleError2: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure){
    case "Error" => monadErrorEither.pure(43)
    case "Another error" => Right(34)
    case _ => Left("Error")
  }

  val filteredSuccess: ErrorOr[Int] = monadErrorEither.ensure(success)("Number is too small")(_ < 100)

  import cats.instances.try_._
  val exception = new RuntimeException("Really bad")
  val pureException: Try[Nothing] = MonadError[Try, Throwable].raiseError(exception)

  import cats.instances.future._
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureError: Future[Nothing] = MonadError[Future, Throwable].raiseError(exception)


  type ErrorsOr[T] = Validated[List[String],T]
  import cats.ApplicativeError
  import cats.instances.list._
  val applErrorVal = ApplicativeError[ErrorsOr, List[String]]

  import cats.syntax.applicative._
  import cats.syntax.applicativeError._
  val extendedSuccess = 42.pure[ErrorsOr]
  val extendedError: ErrorsOr[Int] = List("Error").raiseError[ErrorsOr, Int]
  val recoveredError = extendedError.recover{
    case _ => 43
  }

  import cats.syntax.monadError._
  val testedSuccess = success.ensure("Some error")(_ > 100)

  def main(args: Array[String]): Unit = {

  }
}
