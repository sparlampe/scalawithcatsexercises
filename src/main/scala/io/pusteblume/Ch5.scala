package io.pusteblume


import scala.util.chaining.scalaUtilChainingOps

object MonadTransformer extends App{
  import cats.data.EitherT
  import scala.concurrent.Future
  import cats.instances.future._
  import scala.concurrent.ExecutionContext.Implicits.global

  type Response[A] = EitherT[Future,String,A]
  def getPowerLevel(autobot: String): Response[Int] ={
    Map(
      "Jazz" -> 6,
      "Bumblebee" -> 8,
      "Hot Rod" -> 10
    ).get(autobot) match {
      case Some(level) => EitherT.right(Future(level))
      case None => EitherT.left(Future(s"$autobot is unresponsive"))
    }
  }

  def tacticalReport(ally1: String, ally2: String): String = {
   val fut = (for {
      l1 <- getPowerLevel(ally1)
      l2 <- getPowerLevel(ally2)
    } yield l1+l2 > 15
      ).value
    import scala.concurrent.Await
    import scala.concurrent.duration._
    
    Await.result(fut, 1.second) match {
      case Right(true) =>  s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) => s"$ally1 and $ally2 need a recharge."
      case Left(err) => err
    }
  }
    
  
  
  tacticalReport("Jazz", "Bumblebee") pipe println
  tacticalReport("Bumblebee", "Hot Rod") pipe println
  tacticalReport("Jazz", "Ironhide") pipe println
}