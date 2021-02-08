package me.martinmorek.part4typeclasses

object Aplicatives {

  import cats.Applicative
  import cats.instances.list._
  val listApplicative = Applicative[List]
  val aList = listApplicative.pure(2)

  import cats.instances.option._
  val optionApplicative = Applicative[Option]
  val anOption = optionApplicative.pure(3)

  import cats.syntax.applicative._
  val aSweetList: List[Int] = 4.pure[List]
  val aSweetOption: Option[Int] = 3.pure[Option]

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val aValidValue: ErrorsOr[Int] = Validated.valid(34)
  val aModifiedValidated = aValidValue.map(_ + 10)
  val validatedApplicative = Applicative[ErrorsOr]

  //TODO: productWithApplicatives
//  def ap[W[_], A, B](wf: W[A => B])(wa: W[A]): W[B]= ???
  def productWithApplicatives [W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = {
    val functionWrapper: W[B => (A, B)] = applicative.map(wa)(a => (b:B) => (a,b))
    applicative.ap(functionWrapper)(wb)
  }

  def main(args: Array[String]): Unit = {

  }
}
