package me.martinmorek.part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers {

  def sumAllOptions(list: List[Option[Int]]): Int = ???

  import cats.data.OptionT
  import cats.instances.future._
  import cats.instances.list._

  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'),Option('b'), Option.empty[Char]))

  val listOfTuple: OptionT[List, (Int, Char)] = for {
    number <- listOfNumberOptions
    char <- listOfCharOptions
  } yield (number, char)

  import cats.data.EitherT
  val listOfEither: EitherT[List, String, Int] = EitherT(List(Left("error"), Right(8), Right(234)))

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
  val futureOfEither: EitherT[Future, String, Int] = EitherT.left(Future("error"))

  // TODO:
  val bandwidths = Map(
    "server1.xyz.com" -> 50,
    "server2.xyz.com" -> 300,
    "server3.xyz.com" -> 170
  )
  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case Some(value) => EitherT.right(Future(value))
    case None => EitherT.left(Future(s"Server '$server' is unreachable"))
  }

  // TODO 1:
  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = {
    for {
      bandwidth1 <- getBandwidth(s1)
      bandwidth2 <- getBandwidth(s2)
    } yield bandwidth1 + bandwidth2 >= 250
  }

  // TODO 2:
  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
    canWithstandSurge(s1, s2).transform {
      case Left(reason) => Left(s"Servers $s1 and $s2 cannot cope with incoming traffic spike: $reason")
      case Right(false) => Left(s"Servers $s1 and $s2 cannot cope with incoming traffic spike: not enough bandwidth")
      case Right(true) => Right(s"Servers $s1 and $s2 can cope with incoming spike")
    }

  def main(args: Array[String]): Unit = {
    val trafficReport = generateTrafficSpikeReport("server1.xyz.com", "server.xyz.com").value
    trafficReport.foreach(println)
  }
}
