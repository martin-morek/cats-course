package me.martinmorek.part3datamanipulation

object Evaluation {

  import cats.Eval
  val instantEval: Eval[Int] = Eval.now {
    println("Computing now!")
    343462
  }

  val redoEval: Eval[Int] = Eval.always{
    println("Computing again!")
    67029
  }

  val delayedEval: Eval[Int] = Eval.later{
    println("Computing later!")
    206282
  }

  val composedEval = instantEval.flatMap(value1 => delayedEval.map(value2 => value1 + value2))

  val anotherComposedEvaluation = for {
    value1 <- instantEval
    value2 <- delayedEval
  } yield value1 + value2

  //TODO 1:
  val evalEx1 = for {
    a <- delayedEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d

  val dontRecompute = redoEval.memoize
  val tutorial = Eval
    .always{println("Step 1"); "put the guitar on you lap"}
    .map{step1 => println("Step 2"); s"$step1 then put your left hand on the neck"}
    .memoize
    .map{step12 => println("Step 3"); s"$step12 then with your right hand strike the string"}

  //TODO 2: implement defer such that defer(Eval.now) does NOT run the side effects
  def defer[T](eval: => Eval[T]): Eval[T ] = Eval.later(()).flatMap(_ => eval)

  // TODO 3: rewrite this method with Evals
  def reverseList[T](list: List[T]): List[T] =
    if(list.isEmpty) list
    else reverseList(list.tail) :+ list.head

  def reverseListEval[T](list: List[T]): Eval[List[T]] =
    if(list.isEmpty) Eval.now(list)
    else defer(reverseListEval(list.tail).map(_ :+ list.head))

  def main(args: Array[String]): Unit = {
    println(reverseListEval((1 to 10000).toList).value)
  }

}
