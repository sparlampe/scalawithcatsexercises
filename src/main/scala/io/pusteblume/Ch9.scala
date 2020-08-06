package io.pusteblume

import org.scalatest.FunSuite

import scala.concurrent.{ Future}
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.functor._
import cats.{Applicative, Id}

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

//trait TestUptimeClient extends UptimeClient[Id] {
//  def getUptime(hostname: String): Int
//}

trait RealUptimeClient extends UptimeClient[Future] {
  def getUptime(hostname: String): Future[Int]
}

class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}

class ControllerTest extends FunSuite {

  test("testReceive integration") {
    
    class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id]{
      def getUptime(hostname: String): Int = hosts.getOrElse(hostname, 0)
    }
    
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }
  
}
