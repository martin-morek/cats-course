package me.martinmorek.part3datamanipulation
import cats.kernel.Semigroup

import scala.collection.mutable.ListBuffer
import scala.util.Try

object DataValidation {

  import cats.data.Validated

  val validValue: Validated[String, Int] = Validated.valid(345)
  val invalidValue: Validated[String, Int] = Validated.invalid("Wrong value")
  val aTest: Validated[String, Int] = Validated.cond(2 > 3, 100, "Condition failed")

  // TODO:
  def isPrime(n: Int): Boolean = !Range(2, n - 1).exists(n % _ == 0)

  def testNumber(n: Int): Either[List[String], Int] = {
    val errors: ListBuffer[String] = scala.collection.mutable.ListBuffer()

    if(!isPrime(n)) errors.append(s"$n must be a prime")
    if(n < 0 ) errors.append(s"$n must be no-negative")
    if(n >= 100) errors.append(s"$n must be smaller than 100")
    if(n % 2 != 0) errors.append(s"$n must be even")

    if(errors.isEmpty) Right(n)
    else Left(errors.toList)
  }

  import cats.instances.list._
  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance(Math.max)

  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated.cond(isPrime(n), n, List(s"$n must be a prime"))
      .combine(Validated.cond(n > 0, n, List(s"$n must be no-negative")))
      .combine(Validated.cond(n < 100, n, List(s"$n must be smaller than 100")))
      .combine(Validated.cond(n % 2 == 0, n, List(s"$n must be even")))

  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(34))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("Missing value"))
  val tryFromValidated: Validated[Throwable, Int] = Validated.fromTry(Try("10".toInt))

  val eitherFromValidated: Either[List[String], Int] = eitherToValidated.toEither
  val optionFromValidated: Option[Int] = optionToValidated.toOption

  // TODO 2:
  object FormValidation {
    type FormValidation[T] = Validated[List[String], T]

    def getValue(form: Map[String, String], field: String): FormValidation[String] =
      Validated.fromOption(form.get(field), List(s"The $field must be specified"))

    def isNotBlank(value: String): FormValidation[String] =
      Validated.cond(value.nonEmpty, value, List(s"Value name cannot be blank"))

    def isEmail(value: String): FormValidation[String] =
      Validated.cond(value.contains('@'), value, List(s"Email must contains '@'"))

    def isValidPassword(value: String): FormValidation[String] =
      Validated.cond(value.length >= 10, value, List(s"Password must have at least 10 characters"))

    import cats.instances.string._
    def validateForm(form: Map[String, String]): FormValidation[String] = {
      getValue(form, "name").andThen(isNotBlank)
        .combine(getValue(form, "email")).andThen(isEmail)
        .combine(getValue(form, "password")).andThen(isValidPassword)
        .map(_ => "Success")
    }
  }

  import cats.syntax.validated._
  val aValidValue: Validated[List[String], Int] = 34.valid[List[String]]
  val aInvalidValue: Validated[List[String], Int] = List("Wrong value").invalid[Int]

  def main(args: Array[String]): Unit = {
    val formValidation = FormValidation.validateForm(
      Map(
        "name" -> "Admin",
        "email" -> "admin@email.com",
        "password" -> "superSecretPassword"
      )
    )

    println(formValidation)
  }
}
