package io.pusteblume

trait Printable[A]{
  def format(value: A):String
}

object PrintableInstances {
  implicit val printableSting = new Printable[String] {
    def format(value: String) = value
  }
  
  implicit val printableInt = new Printable[Int] {
    def format(value: Int) = s"$value"
  }
}

object Printable {
  def format[A](value:A)(implicit formatter: Printable[A]): String = formatter.format(value)
  def print[A](value:A)(implicit formatter: Printable[A]): Unit = println(formatter.format(value))
}


object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format (implicit formatter: Printable[A]):String = formatter.format(value)
    def print (implicit formatter: Printable[A]):Unit = println(format(formatter))
  }
}

final case class Cat(name: String, age: Int, color: String)

object Printer extends App {
  import PrintableInstances._
  import PrintableSyntax._

  implicit val printableCat = new Printable[Cat] {
    def format(value: Cat):String = {
      val name = Printable.format(value.name.toUpperCase())
      val age = Printable.format(value.age)
      val color = Printable.format(value.color.toUpperCase)
      s"$name is a $age year-old $color cat."
    }
  }
  
  val cat = Cat("Yankee", 3, "Red")
  Printable.print(cat)
  cat.print
}
object CatsInstances extends App {
  import cats.Show
  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.show._ 
  val showInt: Show[Int] = Show.apply[Int]
  val showString: Show[String] = Show.apply[String]
  val intAsString: String = showInt.show(123)
  val stringAsString: String = showString.show("abc")
  
  
  println(s"$intAsString, $stringAsString")
  println(s"${123.show}, ${"abc".show}")
  
  implicit val catShow: Show[Cat] = Show.show(cat => s"${cat.name.show} is a ${cat.age.show} year-old ${cat.color.show} cat.")
  
  println(Cat("Yankee", 3, "Red").show)
  
}

object EqEx extends App{
  import cats.Eq
  import scala.util.chaining.scalaUtilChainingOps
  import cats.instances.int._
  import cats.instances.string._
  import cats.instances.option._
  import cats.syntax.eq._
  
  List(1, 2, 3).map(Option(_)).map(_.filter(item => item == 1)).pipe(println)
  implicit val dateEq: Eq[Cat] = Eq.instance[Cat] { (cat1, cat2) =>
      cat1.name === cat2.name &&
      cat1.age === cat2.age &&
      cat1.color === cat2.color
  }
  
  println(Cat("Yankee", 3, "Red")===Cat("Yankee", 3, "Red"))
  println(Cat("Yankee", 3, "Red")=!=Cat("Yankee", 33, "Red"))
  println(Option(Cat("Yankee", 3, "Red")) =!= Option.empty[Cat])
}