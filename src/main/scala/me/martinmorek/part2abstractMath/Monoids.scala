package me.martinmorek.part2abstractMath

object Monoids {

  import cats.Monoid
  import cats.instances.int._
  import cats.instances.option._
  import cats.instances.string._

  val intMonoid = Monoid[Int]
  val stringMonoid = Monoid[String]
  val optionMonoid = Monoid[Option[Int]]

  import cats.syntax.monoid._
  val combinedOptionFancy = Option(3) |+| Option(5)

  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T =
    list.foldLeft(monoid.empty)(_ |+| _)

  import cats.instances.map._

  val phonebook = List(
    Map(
      "Alice" -> 345,
      "Tom" -> 235
    ),
    Map(
      "Charlie" -> 244,
      "Jessica" -> 622
    ),
    Map(
      "Joshua" -> 543
    )
  )

  case class ShoppingCart(items: List[String], total: Double)

  import cats.instances.list._

  implicit val shoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance(
    ShoppingCart(List.empty[String], 0.0),
    (x,y) => ShoppingCart(x.items |+| y.items, x.total + y.total)
  )


  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = combineFold(shoppingCarts)

  def main(args: Array[String]): Unit = {
    println(optionMonoid.empty)

    val combineOptions = optionMonoid.combine(Option(2), Option.empty[Int])
    println(combineOptions)

    val intList = (1 to 100).toList
    println(combineFold(intList))

    println(combineFold(phonebook))

    val shoppingCartList = List(
      ShoppingCart(List("iphone", "shoes", "books"), 1500),
      ShoppingCart(List("bicycle, blender"), 300),
      ShoppingCart(List(), 0.0)
    )

    println(checkout(shoppingCartList))
  }

}
