package me.martinmorek.part5alien

import cats.kernel.Monoid

object InvariantFunctors {

  trait Crypto[A] { self =>
    def encrypt(value: A): String
    def decrypt(secret: String): A

    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B] {
      override def encrypt(value: B): String = self.encrypt(back(value))
      override def decrypt(secret: String): B = forth(self.decrypt(secret))
    }
  }

  def encrypt[A](value: A)(implicit crypto: Crypto[A]): String = crypto.encrypt(value)
  def decrypt[A](secret: String)(implicit crypto: Crypto[A]): A = crypto.decrypt(secret)

  implicit val caesarCypher: Crypto[String] = new Crypto[String] {
    override def encrypt(value: String): String = value.map(ch => (ch + 2).toChar)
    override def decrypt(secret: String): String = secret.map(ch => (ch - 2).toChar)
  }

  implicit val doubleCrypto: Crypto[Double] = caesarCypher.imap(_.toString,_.toDouble)

  //TODO 1: support Option[String]
  implicit val optionStringCrypto: Crypto[Option[String]] = caesarCypher.imap(_.getOrElse(""), Option(_))

  //TODO 2: if you have a Crypto[T] => Crypto[Option[T]] if you have Monoid[T] in scope
  implicit def optionCrypto[T](implicit crypto: Crypto[T], monoid: Monoid[T]): Crypto[Option[T]] =
    crypto.imap(_.getOrElse(monoid.empty), Option(_))

  import cats.Invariant
  import cats.Show
  import cats.instances.string._
  val showString = Show[String]
  val showOptionString = Invariant[Show].imap(showString)(Option(_))(_.getOrElse(""))

  import cats.syntax.invariant._
  val showOptionString2 = showString.imap(Option(_))(_.getOrElse(""))

  //TODO 3: what's the relationship?
  trait MyInvariant[W[_]] {
    def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B]
  }

  trait MyContravariant[W[_]] extends MyInvariant [W]{
    def contramap[A, B](wa: W[A])(back: B => A): W[B]

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] =
      contramap(wa)(back)
  }

  trait MyFunctor[W[_]] extends MyInvariant [W]{
    def map[A, B](wa: W[A])(forth: A => B): W[B]

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] =
      map(wa)(forth)
  }

  def main(args: Array[String]): Unit = {
    val secret = encrypt("password")
    val realValue = decrypt[String](secret)

    println(secret)
    println(realValue)
  }

}
