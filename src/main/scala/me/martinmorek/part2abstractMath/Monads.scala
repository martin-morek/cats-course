package me.martinmorek.part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Monads {

  // TODO: 1.1 all combinations
  val charList = List('a','b','c')
  val numberList = List(1,2,3)

  val listCombinations: List[(Int, Char)] = for {
    n <- numberList
    c <- charList
  } yield (n,c)

  // TODO: 1.2 option combination
  val numberOption = Option(2)
  val charOption = Option('d')

  val optionCombination: Option[(Int, Char)] = for {
    n <- numberOption
    c <- charOption
  } yield (n,c)

  // TODO: 1.3 future combination
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val numberFuture = Future(42)
  val charFuture = Future('Z')

  val futureCombination: Future[(Int, Char)] = for {
    n <- numberFuture
    c <- charFuture
  } yield (n,c)

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    // TODO 3: implement map
    def map[A,B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure(f(x)))
  }

  import cats.Monad
  import cats.instances.option._
  val optionMonad = Monad[Option]
  val aOption = optionMonad.pure(2)
  val aTransformedOption = optionMonad.flatMap(aOption)(x => if(x == 2) Some(x) else None)

  import cats.instances.list._
  val listMonad = Monad[List]
  val aList = listMonad.pure(4)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1))

  // TODO: use a Monad[Future]
  import cats.instances.future._
  val futureMonad = Monad[Future]
  val aFuture = futureMonad.pure(42)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x * 4))

  def getPairList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] = numbers.flatMap(n => chars.map(ch => (n, ch)))
  def getPairOption(number: Option[Int], char: Option[Char]): Option[(Int, Char)] = number.flatMap(n => char.map(ch => (n, ch)))
  def getPairFuture(number: Future[Int], char: Future[Char]): Future[(Int, Char)] = number.flatMap(n => char.map(ch => (n, ch)))

  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a,b)))

  import cats.syntax.applicative._
  val oneOption = 1.pure[Option]
  val oneList = 1.pure[List]

  import cats.syntax.flatMap._
  val oneOptionTransformed = oneOption.flatMap(x => (x + 1).pure[Option])

  //TODO 4: implement a shorter version of getPairs using for-comprehensions
  import cats.syntax.functor._
  def getPairsFor[M[_], A, B](ma: M[A], mb:M[B])(implicit monad: Monad[M]): M[(A,B)] = for {
    a <- ma
    b <- mb
  } yield (a,b)

  def main(args: Array[String]): Unit = {
    println(getPairs(numberList, charList))
    println(getPairsFor(numberList, charList))
  }
}
