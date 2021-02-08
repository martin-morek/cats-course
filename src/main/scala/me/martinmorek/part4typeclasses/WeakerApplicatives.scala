package me.martinmorek.part4typeclasses

import cats.{Functor, Semigroupal}

object WeakerApplicatives {

  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {
    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
      val functionWrapper: W[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }

    //TODO:
    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      val tupleW: W[(A, B)] = product(tuple._1, tuple._2)
      map(tupleW){case(a, b) =>
        f(a, b)
      }
    }

    def ap[B, T](wf: W[B => T])(wb: W[B]): W[T]
  }

  trait MyApplicative[W[_]] extends MyApply[W] {
    def pure[A](a: A): W[A] = ???
  }

  import cats.Apply
  import cats.instances.option._
  val applyOption = Apply[Option]
  val funcApp = applyOption.ap(Some((x:Int) => x + 1))(Some(3))

  import cats.syntax.apply._
  val tupleOfOptions: (Option[Int], Option[Int], Option[Int]) = (Option(1), Option(2), Option(3))
  val optionOfTuples: Option[(Int, Int, Int)] = tupleOfOptions.tupled
  val sumOption: Option[Int] = tupleOfOptions.mapN(_ + _ + _)

  def main(args: Array[String]): Unit = {

  }
}
