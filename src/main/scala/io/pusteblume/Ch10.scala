package io.pusteblume

import cats.Semigroup
import cats.syntax.either._
import cats.syntax.semigroup._

import scala.util.chaining.scalaUtilChainingOps // for |+|
final case class CheckF[E, A](func: A => Either[E, A]) {
  def apply(a: A): Either[E, A] =
    func(a)
  def and(that: CheckF[E, A])
         (implicit s: Semigroup[E]): CheckF[E, A] =
    CheckF { a =>
      (this(a), that(a)) match {
        case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
        case (Left(e), Right(_)) => e.asLeft
        case (Right(_), Left(e)) => e.asLeft
        case (Right(_), Right(_)) => a.asRight
      }
    }
}



sealed trait Check[E, A] {
  import Check._
  def and(that: Check[E, A]): Check[E, A] =
    And(this, that)
  def apply(a: A)(implicit s: Semigroup[E]): Either[E, A] =
    this match {
      case Pure(func) =>
        func(a)
      case And(left, right) =>
        (left(a), right(a)) match {
          case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
          case (Left(e), Right(_)) => e.asLeft
          case (Right(_), Left(e)) => e.asLeft
          case (Right(_), Right(_)) => a.asRight
        }
    }
}

object Check {
  final case class And[E, A](
                              left: Check[E, A],
                              right: Check[E, A]) extends Check[E, A]

  final case class Pure[E, A](
                               func: A => Either[E, A]) extends Check[E, A]
  def pure[E, A](f: A => Either[E, A]): Check[E, A] =
    Pure(f)
}






object CheckMain extends App {
  import cats.instances.list._ // for Semigroup
  val a: CheckF[List[String], Int] =
    CheckF { v =>
      if(v > 2) v.asRight
      else List("Must be > 2").asLeft
    }
  val b: CheckF[List[String], Int] =
    CheckF { v =>
      if(v < -2) v.asRight
      else List("Must be < -2").asLeft
    }
  val check: CheckF[List[String], Int] =
    a and b

  check(5) pipe println
  check(0) pipe println


  val aa: CheckF[Nothing, Int] =
    CheckF(v => v.asRight)

  val bb: CheckF[Nothing, Int] =
    CheckF(v => v.asRight)

  //val checkNothing = aa and bb






  val aAdt: Check[List[String], Int] =
    Check.pure { v =>
      if(v > 2) v.asRight
      else List("Must be > 2").asLeft
    }
  val bAdt: Check[List[String], Int] =
    Check.pure { v =>
      if(v < -2) v.asRight
      else List("Must be < -2").asLeft
    }
  val checkAdt: Check[List[String], Int] =
    aAdt and bAdt

  checkAdt(5) pipe println
  checkAdt(0) pipe println

  
}