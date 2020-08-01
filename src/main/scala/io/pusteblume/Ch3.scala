package io.pusteblume




import scala.util.chaining.scalaUtilChainingOps

object FunctorMain extends  App{
  import cats.Functor
  import cats.instances.list._
  import cats.syntax.functor._
  
  val listFunctor = Functor[List]
  val list=List(1,2,3)
  listFunctor.map(list)(_ * 2) pipe println
  val doubler: Int=> Int = x => x * 2
  val listDoubler: List[Int] => List[Int] = listFunctor.lift(doubler)
  listDoubler(list).pipe(println)
  listFunctor.as(list,"1") pipe println
  
  def doMath[F[_]: Functor](start: F[Int]): F[Int] = start.map(x => x + 2)
  import cats.instances.function._
  doMath(doubler) pipe(_(2)) pipe println
}


sealed trait Tree[+A] 
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)
  def leaf[A](value: A): Tree[A] =
    Leaf(value)
}

object TreeFunctorMain extends App{
  import cats.Functor
  
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(left, right) => Branch(map(left)(f),map(right)(f))
      case Leaf(value)=> Leaf(f(value))
    }
  }
  
  
  var branch = Branch(Leaf(2), Leaf(3))
  val leaf = Leaf(3)

  var branchTree = Tree.branch(Tree.leaf(2), Tree.leaf(3))
  val leafTree = Tree.leaf(3)
  
  implicit class FunctorOps[F[_], +A](src: F[A]) {
    def map[B](func: A => B)
              (implicit functor: Functor[F]): F[B] =
      functor.map(src)(func)
  }
  
  Functor[Tree].map(branch)(_ + 2) pipe println
  Functor[Tree].map(leaf)(_ + 2) pipe println
  leafTree.map(_ +2)pipe println
  branchTree.map( x => x + 2) pipe println
}


trait PrintableNew[A] { self =>
  def format(value: A): String
  def contramap[B](func: B => A): PrintableNew[B] = {
    new PrintableNew[B] {
      def format(value: B): String =  self.format(func(value))
    }
  }
}

object ContraMapMain extends App{
  implicit val stringPrintable: PrintableNew[String] =
    new PrintableNew[String] {
      def format(value: String): String =
        s"'${value}'"
    }
  implicit val booleanPrintable: PrintableNew[Boolean] =
    new PrintableNew[Boolean] {
      def format(value: Boolean): String =
        if(value) "yes" else "no"
    }
  def format[A](value: A)(implicit p: PrintableNew[A]): String =
    p.format(value)
    
  format("hello") pipe println
  format(true) pipe println
  
  final case class Box[A](value: A)
  
  implicit def boxPrintable[A](implicit printer: PrintableNew[A]): PrintableNew[Box[A]] = printer.contramap(_.value)

  format(Box("hello world")) pipe println
  format(Box(true)) pipe println
  
}

trait Codec[A] { self =>
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
    import cats.instances.function._
    import cats.syntax.functor._
    override def encode(value: B): String = self.encode(enc(value))
    override def decode(value: String): B = (self.decode _  map  dec)(value)
  }
}

object InvariantFunctor extends App{

  implicit val stringCodec: Codec[String] =
    new Codec[String] {
      def encode(value: String): String = value
      def decode(value: String): String = value
    }

  implicit val doubleCodec: Codec[Double] =
    stringCodec.imap(_.toDouble, _.toString)
  
  final case class Box[A](value: A)

  implicit def boxCode[A](implicit codec: Codec[A]): Codec[Box[A]] = codec.imap(Box(_),_.value )
  
  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)
  def decode[A](value: String)(implicit c: Codec[A]): A =
    c.decode(value)

  encode(123.4) pipe println
  decode[Double]("123.4") pipe println
  encode(Box(123.4)) pipe println
  decode[Box[Double]]("123.4") pipe println
}
