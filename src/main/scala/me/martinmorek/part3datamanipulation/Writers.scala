package me.martinmorek.part3datamanipulation

object Writers {
  import cats.data.Writer
  val aWriter: Writer[List[String], Int] = Writer(List("Log"), 34)
  val mapWriter = aWriter.map(_ + 1)
  val writtenWriter = aWriter.mapWritten(_ :+ "Another log")
  val bimapWriter = aWriter.bimap(_ :+ "Yet another log", _ + 1)
  val mapBothWriter = aWriter.mapBoth((l, i ) => (l :+ i , i * 10))

  import cats.instances.vector._
  val writerA = Writer(Vector("Log A1", "Log A2"), 44)
  val writerB = Writer(Vector("Log B1"), 2354)

  val composedWriter = for {
    a <- writerA
    b <- writerB
  } yield a + b

  import cats.instances.list._
  val anEmptyWriter = aWriter.reset

  val desiredValue = aWriter.value
  val desiredLog = aWriter.written
  val (log, value) = aWriter.run

  // TODO 1:
  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    def run(writer: Writer[Vector[String], Int]): Writer[Vector[String], Int] = {
      if (writer.value <= 0)
        writer.bimap(_ :+ "starting", _ => writer.value)
      else
        run(writer.bimap(_ :+ writer.value.toString, _ -1))
    }
    run(Writer(Vector.empty[String], n))
  }

  // TODO 2:
  def naiveSum(n: Int): Int = {
    if(n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum ${n - 1} = $lowerSum")
      lowerSum + n
    }
  }

  def sumWithLog(n: Int): Writer[Vector[String], Int] =
    if(n <= 0) Writer(Vector.empty[String], 0)
    else {
      for {
        _ <- Writer(Vector(s"Now at $n"), n)
        lowerSum <- sumWithLog(n - 1)
        _ <- Writer(Vector(s"Computed sum ${n - 1} = $lowerSum"), n)
      } yield lowerSum + n
    }

  def main(args: Array[String]): Unit = {
    naiveSum(100)
    sumWithLog(100).written.foreach(println)
  }
}
