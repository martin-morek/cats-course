package me.martinmorek.part2abstractMath

object Functors {

  val aModifiedList = List(1,2,3).map(_ + 1)

  trait MyFunctor[F[_]]{
    def map[A,B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list._
  val listFunctor = Functor[List]
  val incrementedNumber = listFunctor.map(List(1,2,3))(_ + 1)

  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)

  trait Tree[+T]
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  object Tree{
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(value, left, right) => Branch(f(value), map(left)(f), map(right)(f))
      case Leaf(value) => Leaf(f(value))
    }
  }

  import cats.syntax.functor._
  def do10XBetter[F[_]:Functor](container: F[Int]): F[Int] = container.map(_ * 10)

  def main(args: Array[String]): Unit = {
    println(do10x(List(1,2,3,4,5)))

    val intTree = Tree.branch(10, Tree.leaf(100), Tree.leaf(23))
    println(do10x(intTree))
  }
}
