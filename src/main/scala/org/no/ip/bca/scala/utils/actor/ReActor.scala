package org.no.ip.bca.scala.utils.actor

import scala.actors.Actor
import scala.actors.Actor.{loop, react}

object ReActor {
  def reAct(f: PartialFunction[Any, Unit]): ReActor = {
    def rea = new ReActor {
      def reAct = f
    }
    rea.start
    rea
  }
}

trait ReActor extends Actor {
  private type PF = PartialFunction[Any, Unit]
  private case object Stop
  
  trapExit = true
  
  private val handleStop: PF = {
    case Stop =>
      doStop
      exit
  }
  private def safePartial(act: PF) = new PF {
    private val f = act orElse handleStop
    override def isDefinedAt(a: Any) = f isDefinedAt a
    override def apply(a: Any) = try {
      f(a)
    } catch {
      case e: Exception => handleException(e)
    }
  }
  
  def init: Unit = {}
  final def act = {
    init
    loop(to(reAct))
  }
  final def stop = {
    this ! Stop
  }
  def reAct: PF
  final def to(p: PF) = react(safePartial(p))
  
  def doStop: Unit = {}
  def handleException(e: Exception) = {}
}

trait StopOnException extends ReActor {
  override def handleException(e: Exception) = {
    super.handleException(e)
    stop
  }
}