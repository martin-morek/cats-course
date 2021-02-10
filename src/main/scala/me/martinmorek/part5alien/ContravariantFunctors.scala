package me.martinmorek.part5alien

import cats.kernel.Monoid

object ContravariantFunctors {

  trait Format[T] { self =>
    def format(value: T): String

    def contramap[A](func: A => T): Format[A] = new Format[A] {
      override def format(value: A): String = self.format(func(value))
    }
  }

  def format[A](value: A)(implicit f: Format[A]) = f.format(value)

  implicit object StringFormat extends Format[String] {
    override def format(value: String): String = "\"" + value + "\""
  }

  implicit object IntFormat extends Format[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit object BooleanFormat extends Format[Boolean] {
    override def format(value: Boolean): String = if(value) "Y" else "N"
  }

  import cats.instances.option._
  implicit def getOptionFormat[T](implicit f: Format[T], monoid: Monoid[T]): Format[Option[T]] =
    f.contramap[Option[T]](_.getOrElse(monoid.empty))

  import cats.Contravariant
  import cats.Show
  import cats.instances.int._

  val showInt = Show[Int]
  val showOptionInt: Show[Option[Int]] = Contravariant[Show].contramap(showInt)(_.getOrElse(0))

  import cats.syntax.contravariant._
  val showOptionIntShorter: Show[Option[Int]] = showInt.contramap(_.getOrElse(0))

  def main(args: Array[String]): Unit = {
    println(format("Tom"))
    println(format(343))
    println(format(true))
    println(format(Option(42)))
    println(format(Option(Option(42))))
  }

}
