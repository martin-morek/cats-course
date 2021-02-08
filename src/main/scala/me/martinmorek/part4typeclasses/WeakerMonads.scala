package me.martinmorek.part4typeclasses

import cats.{Applicative, Apply, FlatMap}

object WeakerMonads {

  trait MyFlatMap[M[_]] extends Apply[M]{
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def ap[A, B](wf: M[A => B])(fa: M[A]): M[B] =
      flatMap(fa)(a => map(wf)(f => f(a)))
  }

  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M]{
    override def map[A,B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure(f(x)))
  }

  import cats.syntax.functor._
  import cats.syntax.flatMap._

  def getPairs[M[_]: FlatMap, A, B](as: M[A], bs: M[B]): M[(A, B)] =
    for{
      a <- as
      b <- bs
    } yield (a,b)

  def main(args: Array[String]): Unit = {

  }
}
