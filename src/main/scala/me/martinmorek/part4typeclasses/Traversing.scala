package me.martinmorek.part4typeclasses

import cats.{Applicative, Foldable, Functor, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Traversing {

  implicit val ex: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
  val servers: List[String] = List("test-server.xyz.com", "prod-server.xzy.com", "ci-server-1.xzy.com")
  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length + 100)

  /*
  we
  - a List[String]
  - a function String => Future[Int]
  we want Future[List[Int]]
   */
  val allBandwidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int]) ){ (accumulator, hostname) =>
    for {
      bandwidth <- getBandwidth(hostname)
      acc <- accumulator
    } yield acc :+ bandwidth
  }

  val allBandwidthsTraverse: Future[List[Int]] = Future.traverse(servers)(getBandwidth)
  val allBandwidthsSequence = Future.sequence(servers.map(getBandwidth))

  // TODO 1:
  import cats.syntax.applicative._
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  def listTraverseWithMonad[F[_]: Monad, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) {
      (accumulator, current) =>
        for{
          newElem <- func(current)
          acc <- accumulator
        } yield acc :+ newElem
  }

  import cats.syntax.apply._
  def listTraverseWithApply[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) {
      (accumulator, current) =>
        val newElement = func(current)
        (accumulator, newElement).mapN(_ :+ _)
  }

  // TODO 2:
  def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
    listTraverseWithApply(list)(identity)

  def listSequenceManual[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
    list.foldLeft((List.empty[A]).pure[F]){
      (accumulator, current) =>(accumulator, current).mapN(_ :+ _)
    }

  // TODO 3:
  import cats.instances.vector._
  val allPairs = listSequence(List(Vector(1,2), Vector(3,4)))
  val allTriples = listSequence(List(Vector(1,2), Vector(3,4), Vector(5,6)))

  import cats.instances.option._
  def filerAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
    listTraverseWithApply[Option, Int, Int](list)(n => Some(n).filter(predicate))

  // TODO 4:
  val allTrue = filerAsOption(List(2,4,6))(_ % 2 == 0)
  val someFalse = filerAsOption(List(1,2,3))(_ % 2 == 0)

  import cats.data.Validated
  import cats.instances.list._

  type ErrorsOr[T] = Validated[List[String], T]
  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverseWithApply[ErrorsOr, Int, Int](list){ n =>
      if(predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"predicate for $n failed"))
    }

  // TODO 5:
  val allTrueValidated = filterAsValidated(List(2,4,6))(_ % 2 == 0)
  val someFalseValidated = filterAsValidated(List(1,2,3))(_ % 2 == 0)

  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L]{
    def traverse[F[_]: Applicative, A, B](container: L[A])(func: A => F[B]): F[L[B]]
    def sequence[F[_]: Applicative, A](container: L[F[A]]): F[L[A]] = traverse(container)(identity)

    // TODO 6:
    import cats.Id
    def map[A, B](wa: L[A])(f: A => B): L[B] = traverse[Id, A, B](wa)(f)
  }

  import cats.Traverse
  import cats.instances.future._
  val allBandwidthsWithCats = Traverse[List].traverse(servers)(getBandwidth)

  import cats.syntax.traverse._
  val allBandwidthsWithCats2 = servers.traverse(getBandwidth)

  def main(args: Array[String]): Unit = {
    println(allPairs)
    println(allTriples)
    println(allTrue)
    println(someFalse)
    println(allTrueValidated)
    println(someFalseValidated)
  }

}
