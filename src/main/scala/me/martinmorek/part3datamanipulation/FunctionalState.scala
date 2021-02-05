package me.martinmorek.part3datamanipulation

object FunctionalState {
  type MyState[S,A] = S => (S, A)

  import cats.data.State
  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"counted: $currentCount"))
  val (eleven, counted10) = countAndSay.run(10).value


  val firstTransformation = State((s: Int) => (s+1, s"Added 1 to $s, obtained ${s + 1}"))
  val secondTransformation = State((s: Int) => (s*5, s"Multiplied $s by 5, obtained ${s * 5}"))
  val compositeTransformation: State[Int, (String, String)] = firstTransformation.flatMap {first =>
    secondTransformation.map(second => (first, second))
  }

  val compositeTransformationFor = for {
    first <- firstTransformation
    second <- secondTransformation
  } yield (first, second)

  // TODO 1 :
  case class ShoppingCart(items: List[String], total: Double)
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] =State(
    cart => (ShoppingCart(item :: cart.items, cart.total + price), cart.total + price))

  val cart = for {
    _ <- addToCart("phone", 790)
    _ <- addToCart("book", 23)
    total <- addToCart("chair", 100)
  } yield total

  // TODO 2:
  def inspect[A, B](f: A => B): State[A, B] = State((s: A) => (s, f(s)))
  def get[A]: State[A ,A] = State((s: A) => (s, s))
  def set[A](value: A): State[A, Unit] = State((_: A) => (value, ()))
  def modify[A](f: A => A): State[A, Unit] = State((s: A) => (f(s), ()))

  val program = for {
    a <- get[Int]
    _ <- set[Int](10)
    b <- get[Int]
    _ <- modify[Int](_ + 43)
    c <- inspect[Int, Int](_ * 2)
  } yield (a, b, c)

  def main(args: Array[String]): Unit = {
    println(compositeTransformationFor.run(10).value)
    println(cart.run(ShoppingCart(List.empty, 0.0)).value)
  }
}
