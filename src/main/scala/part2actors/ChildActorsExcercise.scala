package part2actors

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.BackoffSupervisor.CurrentChild
import part2actors.ChildActorsExcercise.WordCounterMaster.Initialize

object ChildActorsExcercise extends App {

  object  WordCounterMaster{
    case class Initialize(nChildren: Int)
    case class WordCountTask(task: String)
    case class WordCountReply(count: Int)
  }

  // Distributed Word counting
  class WordCounterMaster extends Actor {
    import WordCounterMaster._
    override def receive: Receive = {
      case Initialize(n) =>
      val childRefs = for(i <- 0 to n)  yield context.actorOf(Props[WordCounterWorker], s"wcw_${i}")
      context.become(withWorkers(childRefs, 0))
    }

    def withWorkers(actorRefs: Seq[ActorRef], currentChild: Int) : Receive = {
      case WordCountTask(task) => actorRefs(currentChild) ! WordCountTask(task)
        val nextChild = (currentChild) + 1 % actorRefs.length
        context.become(withWorkers(actorRefs,nextChild))
      case WordCountReply(count) =>
    }
  }


  class WordCounterWorker extends Actor {
    import  WordCounterMaster._
    override def receive: Receive = {
      case WordCountTask(text) => sender() ! WordCountReply(text.split(" ").length)
    }
  }


}
