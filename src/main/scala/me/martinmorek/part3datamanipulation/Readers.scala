package me.martinmorek.part3datamanipulation

object Readers {

  case class Configuration(dbUsername: String, dbPassword: String, host: String, port: Int, nThreads: Int, replyTo: String)
  case class DbConnection(username: String, password: String){
    def getOrderStatus(orderId: Long): String = "Shipped!"
    def getLastOrderId(username: String): Long = 245321L
  }
  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started...")
  }

  val config = Configuration("admin", "123456", "localhost", 8080, 8, "no-reply@email.com")

  import cats.data.Reader
  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))
  val dbCon = dbReader.run(config)

  def getLastOrderStatus(username: String): String = {
    val userOrderIdReader = dbReader.map(_.getLastOrderId(username))
      .flatMap(id => dbReader.map(_.getOrderStatus(id)))

    userOrderIdReader.run(config)
  }

  // TODO:
  case class EmailService(replyTo: String) {
    def sendEmail(address: String, content: String) =
      s"From: $replyTo, to: $address >>> $content"
  }

  val emailServiceReader: Reader[Configuration, EmailService] = Reader(conf => EmailService(conf.replyTo))

  def emailUser(userName: String, userEmail: String) = {
    val emailReader = for {
      id <- dbReader.map(_.getLastOrderId(userName))
      status <- dbReader.map(_.getOrderStatus(id))
      email <- emailServiceReader.map(_.sendEmail(userEmail, s"Your last order status: $status"))
    } yield email

    emailReader.run(config)
  }

  def main(args: Array[String]): Unit = {
    println(emailUser("Tom", "tommy@example.com"))
  }

}
