package me.martinmorek.part2abstractMath

object Semigroups {

  import cats.Semigroup
  import cats.instances.int._

  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)

  case class Expense(id: Long, amount: Double)
  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense]((exp1, exp2) =>
    Expense(Math.max(exp1.id, exp2.id), exp1.amount + exp2.amount)
  )

  import cats.syntax.semigroup._
  val combinedInts = 1 |+| 4
  val combinedExpenses = Expense(3L, 34) |+| Expense(45L, 45)

  def reduceThing2[T : Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    val expenses = List(Expense(1L, 230), Expense(30L, 70), Expense(10L, 200))
    println(reduceThings(expenses))
  }

}
