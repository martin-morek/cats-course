package me.martinmorek.part4typeclasses

import cats.{Eval, Monoid}

object Folding {

  // TODO: implement in terms of foldLeft
  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] =
      list.foldLeft(List.empty[B])((acc: List[B], a: A) => acc :+ f(a))

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldLeft(List.empty[B])((acc: List[B], a: A) => acc.concat(f(a)))

    def filter[A](list: List[A])(predicate: A => Boolean): List[A] =
      list.foldLeft(List.empty[A])((acc: List[A], a: A) =>
        if (predicate(a)) acc :+ a
        else acc
      )

    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldLeft(monoid.empty)(monoid.combine)
  }

  import cats.Foldable
  import cats.instances.list._
  val sumListLeft = Foldable[List].foldLeft(List(1,2,3), 0)(_ + _)

  import cats.instances.option._
  val sumOptionLeft = Foldable[Option].foldLeft(Option(2), 100)(_ + _)

  val sumListRight: Eval[Int] = Foldable[List].foldRight(List(1,2,3), Eval.now(0)){ (a, eval) =>
    eval.map(_ + a)
  }

  import cats.instances.int._
  val anotherSum = Foldable[List].combineAll(List(1,2,3))
  import cats.instances.string._
  val mappedConcat = Foldable[List].foldMap(List(1,2,3))(_.toString)

  import cats.instances.vector._
  val nestedInt = List(Vector(1,2,3), Vector(4,5,6))
  (Foldable[List] compose Foldable[Vector]).combineAll(nestedInt)

  import cats.syntax.foldable._
  List(1,2,3).combineAll

  def main(args: Array[String]): Unit = {
    import ListExercises._

    println(map(List(1,2,3))(_ + 1))
    println(flatMap(List(1,2,3))(x => List(x + 1)))
    println(filter(List(1,2,3))(_ >= 2))
    println(combineAll(List(1,2,3,4,5)))
  }

}
