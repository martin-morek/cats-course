package me.martinmorek.part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

object MonadPractice {

  import cats.Monad
  import cats.instances.list._
  import cats.instances.option._

  val monadList = Monad[List]
  val aSimpleList = monadList.pure(5)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x+1))

  val aManualEither: Either[String, Int] = Right(42)
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._
  val loadingEither = Monad[LoadingOr]
  val anEither = loadingEither.pure(42)
  val aChangedLoading = loadingEither.flatMap(anEither)(x => if(x == 42) Right(42) else Left("Still loading..."))

  case class OrderStatus(orderId: Long, status: String)
  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(OrderStatus(orderId, "Ready to ship"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if(orderStatus.orderId < 1000) Left("Not available yet")
    else Right("Amsterdam, NL")

  val orderId = 463L

  val orderingLocation = loadingEither.flatMap(getOrderStatus(orderId))(status => trackLocation(status))
  val orderingLocationBetter = getOrderStatus(orderId).flatMap(trackLocation)
  val orderingLocationFor = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

  // TODO
  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "8080"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def getResponse[M[_]](service: HttpService[M], payload: String)(implicit monad: Monad[M]): M[String] = for {
    connection <- service.getConnection(config)
    response <- service.issueRequest(connection, payload)
  } yield (response)


  object HttpServiceAsOption extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] = for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): Option[String] = {
      if(payload.length < 20 )
        Option(s"request ($payload) has been accepted")
      else None
    }
  }

  object HttpServiceAsTry extends HttpService[Try] {
    override def getConnection(cfg: Map[String, String]): Try[Connection] = {
        for {
          host <- cfg.get("host")
          port <- cfg.get("port")
        } yield Connection(host, port)
      }.map(Success(_)).getOrElse(throw new Exception("Cannot load configuration"))

    override def issueRequest(connection: Connection, payload: String): Try[String] = {
      if(payload.length < 20 )
        Success(s"request ($payload) has been accepted")
      else throw new Exception("Request issuing failed")
    }
  }

  implicit val ex: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  object HttpServiceAsFuture extends HttpService[Future] {
    override def getConnection(cfg: Map[String, String]): Future[Connection] = {
      for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield Connection(host, port)
    }.map(Future(_)).getOrElse(throw new Exception("Cannot load configuration"))

    override def issueRequest(connection: Connection, payload: String): Future[String] = {
      if(payload.length < 20 )
        Future(s"request ($payload) has been accepted")
      else Future.failed(throw new Exception("Request issuing failed"))
    }
  }

  object HttpServiceAsEither extends HttpService[LoadingOr] {
    override def getConnection(cfg: Map[String, String]): LoadingOr[Connection] = {
      for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield Connection(host, port)
    }.map(Right(_)).getOrElse(Left("Cannot load configuration"))

    override def issueRequest(connection: Connection, payload: String): LoadingOr[String] = {
      if(payload.length < 20 )
        Right(s"request ($payload) has been accepted")
      else Left("Request issuing failed")
    }
  }

  def main(args: Array[String]): Unit = {
    println(getResponse(HttpServiceAsOption, "Hello option"))
  }
}
