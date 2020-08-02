package io.pusteblume

import scala.util.chaining.scalaUtilChainingOps

object MonadMain extends  App{
  import cats.Monad
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap
  import cats.instances.option._
  import cats.instances.list._
  
  type Id[A] = A
  
  implicit def idMonad: Monad[Id] = new Monad[Id] {
     override def pure[A](x: A): Id[A] = x
     def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)
     def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] = ???
  }
  
  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    a.flatMap(x => b.map(y => x*x + y*y))
    
  sumSquare(Option(3), Option(4)) pipe println
  sumSquare(List(1, 2, 3), List(4, 5)) pipe println
  
  val a = Monad[Id].pure(3)
  val b = Monad[Id].flatMap(a)(_ + 1)

  for {
    x <- a
    y <- b
  } yield x + y pipe println
  
}

object MonadErrorMain extends App {
  import cats.MonadError
  import cats.syntax.applicative._
  import scala.util.Try
  import cats.instances.try_._
  import cats.instances.either._
  
  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] = 
    me.ensure(age.pure[F])(new IllegalArgumentException("Age must be greater than or  equal to 18"))(_ >= 18)

  validateAdult[Try](18) pipe println
  validateAdult[Try](8)  pipe println
  type ExceptionOr[A] = Either[Throwable, A]
  validateAdult[ExceptionOr](-1) pipe println
}

object EvalMonadMain extends App {
  import cats.Eval
  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail =>
        fn(head, foldRight(tail, acc)(fn))
      case Nil =>
        acc
    }

  def foldRightStackSafe[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = {

    def foldRight(as: List[A], acc: B)(fn: (A, B) => B): Eval[B] =
      as match {
        case head :: tail =>
          Eval.defer(foldRight(tail, acc)(fn).map(fn(head, _)))
        case Nil =>
          Eval.now(acc)
      }
    foldRight(as, acc)(fn).value
  }
  foldRightStackSafe((1 to 100000).toList,0L)(_ + _) pipe println
}

object  WriterMain extends App{
  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits._
  import scala.concurrent.duration._
   import cats.data.Writer

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)
  def factorial(n: Int): Writer[Vector[String],Int] = slowly(if(n == 0) Writer(Vector(s"fact $n 1"), 1) else  factorial(n - 1).mapBoth((log, value)=> {
      val newVal = value*n
      (log.appended(s"fact $n $newVal"), newVal)
    }))

  import cats.syntax.writer._
  import cats.data.Writer
  import cats.instances.vector._
  import cats.syntax.applicative._ 
  type Logged[A] = Writer[Vector[String], A]
  
  def factorialBook(n: Int): Logged[Int] =
    for {
      ans <- if(n == 0) {
        1.pure[Logged]
      } else {
        slowly(factorialBook(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  
  Await.result(Future.sequence(Vector(
    Future(factorial(5)),
    Future(factorial(5))
  )), 5.seconds) pipe println

  Await.result(Future.sequence(Vector(
    Future(factorialBook(5)),
    Future(factorialBook(5))
  )), 5.seconds) pipe println
}

 object ReaderMain extends  App{
   import cats.data.Reader
   import cats.syntax.applicative._
   
   final case class Db(usernames: Map[Int, String], passwords: Map[String, String])
   type DbReader[A] = Reader[Db,A]
   
   def findUsername(userId: Int): DbReader[Option[String]] = Reader(db => db.usernames.get(userId))
   def checkPassword(username: String, password: String): DbReader[Boolean] = Reader(db => db.passwords.get(username).contains(password))
   def checkLogin(userId: Int, password: String): DbReader[Boolean] = 
     for {
       userName <- findUsername(userId)
       auth <- userName.map{x => checkPassword(x, password)}.getOrElse{false.pure[DbReader]}
     } yield auth

   val users = Map(
     1 -> "dade",
     2 -> "kate",
     3 -> "margo"
   )
   

   val passwords = Map(
     "dade" -> "zerocool",
     "kate" -> "acidburn",
     "margo" -> "secret"
   )
   val db = Db(users, passwords)
   checkLogin(1, "zerocool").run(db) pipe println
   checkLogin(4, "davinci").run(db) pipe println
 }

object StateMonadMain extends App{
  import cats.data.State
  val a = State[Int, String]{ state =>
    (state, s"The state is $state")
  }

  a.run(10).value pipe println
  a.runS(10).value pipe println
  a.runA(10).value pipe println


  val step1 = State[Int, String]{ num =>
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  }
  val step2 = State[Int, String]{ num =>
    val ans = num * 2
    (ans, s"Result of step2: $ans")
  }
  val both = for {
    a <- step1
    b <- step2
  } yield (a, b)
  val (state, result) = both.run(20).value
  (state, result) pipe println

  val getDemo = State.get[Int]
  getDemo.run(10).value pipe println

  val setDemo = State.set[Int](30)//.map(_ =>(40,20))
  setDemo.run(10).value pipe println

  val pureDemo = State.pure[Int, String]("Result")//.map(_ =>(40,20))
  pureDemo.run(10).value pipe println

  val inspectDemo = State.inspect[Int, String](x => s"${x}!")
  inspectDemo.run(10).value pipe println

  val modifyDemo = State.modify[Int](_ + 1)
  modifyDemo.run(10).value pipe println
  import State._
  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 1)
    b <- get[Int]
    _ <- modify[Int](_ + 1)
    c <- inspect[Int, Int](_ * 1000)
  } yield (a, b, c)
  
   program.run(1).value  pipe println


  type CalcState[A] = State[List[Int], A]
  def evalOne(sym: String): CalcState[Int] = {
    val operator: ((Int, Int) => Int) => CalcState[Int] = fun => {
      State[List[Int], Int] {
        case a :: b :: t =>
          val res = fun(b, a)
          (res :: t, res)
        case _ =>
          sys.error("Fail!")
      }
    }
    
    val operand: Int=>CalcState[Int] = num => State[List[Int],Int](s =>(num :: s, num))
    sym match {
      case "+" => 
        operator(_ + _)
      case "-" =>
        operator(_ - _)
      case "*" =>
        operator(_ * _)
      case "/" =>
        operator(_ / _)
      case num =>
        operand(num.toInt)
    }
  }

  evalOne("42").runA(Nil).value pipe println

  val program1 = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    ans <- evalOne("+")
  } yield ans

   program1.run(Nil).value  pipe println
  import cats.syntax.applicative._
  def evalAll(input: List[String]): CalcState[Int] = input.foldLeft(0.pure[CalcState]) { (a, b) =>
    a.flatMap(_ => evalOne(b))
  }

  val multistageProgram = evalAll(List("1", "2", "+", "3", "*"))
    multistageProgram.run(Nil).value pipe println


  val biggerProgram = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans
    biggerProgram.runA(Nil).value   pipe println

  def evalInput(input: String): Int = (input.split(" ").toList pipe evalAll).runA(Nil).value
   
  evalInput("1 2 + 5 *")  pipe println
}

object TreeMonadMain extends App{
  
  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A])
    extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)
  def leaf[A](value: A): Tree[A] =
    Leaf(value)


  import cats.Monad
  implicit val treeMonad = new Monad[Tree] {
    def flatMap[A, B](value: Tree[A])
                     (fn: A => Tree[B]): Tree[B] = 
      value match {
        case Branch(l,r) => Branch(flatMap(l)(fn), flatMap(r)(fn))
        case Leaf(v) => fn(v)
      }
 
    def pure[A](value: A): Tree[A] = Leaf(value)
      
    
    def tailRecM[A, B](a: A)
                      (fn: A => Tree[Either[A, B]]): Tree[B] =
      flatMap(fn(a)) {
          case Left(a1) => tailRecM(a1)(fn)
          case Right(b) => Leaf(b)
      }
  }


  import cats.syntax.flatMap._ // for flatMap
  branch(leaf(100), leaf(200)).flatMap(x => branch(leaf(x - 1), leaf(x + 1))) pipe println

  import cats.syntax.functor._
  
  (for {
    a <- branch(leaf(100), leaf(200))
    b <- branch(leaf(a - 10), leaf(a + 10))
    c <- branch(leaf(b - 1), leaf(b + 1))
  } yield c) pipe println
  
}