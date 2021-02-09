package me.martinmorek.part4typeclasses

import cats.{Applicative, Monad}

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
  listSequence(List(Vector(1,2), Vector(3,4)))
  listSequence(List(Vector(1,2), Vector(3,4), Vector(5,6)))


  def main(args: Array[String]): Unit = {

  }

}
