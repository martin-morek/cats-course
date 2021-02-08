package me.martinmorek.part4typeclasses

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Semigroupals {

  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option._

  val optionSemigroupal = Semigroupal[Option]
  val aTupledOption: Option[(Int, String)] = optionSemigroupal.product(Some(21), Some("text"))
  val noneTupledOption: Option[(Nothing, Int)] = optionSemigroupal.product(None, Some(34))

  import cats.instances.future._
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aTupledFuture: Future[(String, Int)] = Semigroupal[Future].product(Future("text"), Future(34))

  import cats.instances.list._
  val aTupledList: List[(Int, String)] = Semigroupal[List].product(List(1,2,3), List("a", "b", "c"))

  //TODO: implement
  import cats.Monad
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  def productWithMonads[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr]
  val invalidCombination = validatedSemigroupal.product(
    Validated.invalid(List("Some error")),
    Validated.invalid(List("Some another error"))
  )

  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either._
  val eitherSemigroupal = Semigroupal[EitherErrorsOr]
  val eitherCombination = eitherSemigroupal.product(
    Left(List("Some error")),
    Left(List("Some another error"))
  )

  //TODO: define Semigroupal[List] which does a zip
  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)
  }

  def main(args: Array[String]): Unit = {
    println(invalidCombination)
    println(eitherCombination)

    println(aTupledList)
    println(zipListSemigroupal.product(List(1,2,3), List("a", "b", "c")))
  }
}

