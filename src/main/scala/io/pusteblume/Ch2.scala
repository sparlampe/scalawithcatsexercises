package io.pusteblume

trait Semigroup[A] {
  def combine(x: A, y: A): A
}
trait Monoid[A] extends Semigroup[A] {
  def empty: A


}
object Monoid {
  def apply[A](implicit monoid: Monoid[A]) =
    monoid

  def associativeLaw[A](x: A, y: A, z: A)
                       (implicit m: Monoid[A]): Boolean = {
    m.combine(x, m.combine(y, z)) ==
      m.combine(m.combine(x, y), z)
  }
  def identityLaw[A](x: A)
                    (implicit m: Monoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) &&
      (m.combine(m.empty, x) == x)
  }
}


object BooleanMonoids {
  val andMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true
    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  val orMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false
    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  val xorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false
    override def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)
  }

  val norMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true
    override def combine(x: Boolean, y: Boolean): Boolean = (!x || y) && (x || !y)
  }
  
}

object BooleanMonoidsMain extends App{
  println(Monoid.associativeLaw(true,true,true)(BooleanMonoids.andMonoid))
  println(Monoid.associativeLaw(false,true,false)(BooleanMonoids.andMonoid))

  println(Monoid.associativeLaw(true,true,true)(BooleanMonoids.orMonoid))
  println(Monoid.associativeLaw(false,true,false)(BooleanMonoids.orMonoid))

  println(Monoid.associativeLaw(true,true,true)(BooleanMonoids.xorMonoid))
  println(Monoid.associativeLaw(false,true,false)(BooleanMonoids.xorMonoid))

  println(Monoid.associativeLaw(true,true,true)(BooleanMonoids.norMonoid))
  println(Monoid.associativeLaw(false,true,false)(BooleanMonoids.norMonoid))
}


object SetMonoidsSemigroups {
  def unionSetMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty
    override def combine(x: Set[A], y: Set[A]): Set[A] = x union  y
  }

  def symDiffMonoid[A]: Monoid[Set[A]] =  new Monoid[Set[A]] {
    override  def empty: Set[A] = Set.empty
    override  def combine(a: Set[A], b: Set[A]): Set[A] = (a diff b) union (b diff a)
  }
  
  def intersectionSemigroup[A]: Semigroup[Set[A]] = new Semigroup[Set[A]] {
    override def combine(x: Set[A], y: Set[A]): Set[A] = x  intersect y
  }
  
}

object SetMonoidsMain extends App{
  println(Monoid.associativeLaw(Set.empty[Int],Set(1,2),Set(1,4))(SetMonoidsSemigroups.unionSetMonoid))
  println(Monoid.associativeLaw(Set(234652536),Set(1,2),Set(1,4))(SetMonoidsSemigroups.symDiffMonoid))
  
}


object AddAllThings extends App{
  import cats.instances.int._
  import cats.instances.double._
  import cats.instances.option._
  import cats.syntax.semigroup._
  import cats.syntax.option._
  
  def add1(items: List[Int]): Int = items match {
    case h::t => cats.Monoid[Int].combine(h, add1(t))
    case _ => cats.Monoid[Int].empty
  }

  def add[A: cats.Monoid](items: List[A]): A = items.foldLeft(cats.Monoid[A].empty)(_ |+| _)

  case class Order(totalCost: Double, quantity: Double)
  implicit val orderMonoid: cats.Monoid[Order] = new cats.Monoid[Order] {
    override def empty: Order = Order(0,0)

    override def combine(x: Order, y: Order): Order = Order(x.totalCost |+| y.totalCost, x.quantity |+| y.quantity)
  } 
  
  println(add(List(1.some, Some(2), Some(3))))

  println(add(List(Order(0,3), Order(1,1), Order(2,3))))
 }