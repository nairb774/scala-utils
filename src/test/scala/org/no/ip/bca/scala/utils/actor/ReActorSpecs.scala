package org.no.ip.bca.scala.utils.actor

import java.util.concurrent._
import org.specs._

object ReActorSpec extends ReActorTest
class ReActorTest extends SpecificationWithJUnit {
  private case object Foo
  
  "init" should {
    val cdl = new CountDownLatch(1)
    "be run on start" >> {
      var inited = false
      val r = new ReActor {
        def reAct = { case Foo => }
        override def init = {
          inited = true
          cdl.countDown
        }
      }
      r.start
      cdl.await(1, TimeUnit.SECONDS)
      try {
        inited mustBe true
      } finally {
        r.stop
      }
    }
    
    "not be run before start" >> {
      var inited = false
      val r = new ReActor {
        def reAct = { case Foo => }
        override def init = {
          inited = true
          cdl.countDown
        }
      }
      inited mustBe false
    }
  }
  "exceptions" should {
    val cdl = new CountDownLatch(1)
    "be handled" >> {
      val e = new Exception("Foo")
      var exception: Exception = null
      val r = new ReActor {
        def reAct = { case Foo => throw e }
        override def handleException(exc: Exception) = {
          exception = exc
          cdl.countDown
        }
      }
      r.start
      r ! Foo
      r.stop
      cdl.await(1, TimeUnit.SECONDS)
      exception mustBe e
    }
  }
  "stop" should {
    val cdl = new CountDownLatch(1)
    "trigger a call to doStop" >> {
      var stopped = false
      val r = new ReActor {
        def reAct = { case Foo => }
        override def doStop = {
          stopped = true
          cdl.countDown()
        }
      }
      r.start
      r.stop
      cdl.await(1, TimeUnit.SECONDS)
      stopped mustBe true
    }
  }
}
