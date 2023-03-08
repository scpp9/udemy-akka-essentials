package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.BackoffSupervisor.CurrentChild
import sun.rmi.transport.DGCAckHandler

object ChildActorsExcercise extends App {

  object  WordCounterMaster{
    case class initialize(nChildren: Int)
    case class WordCountTask(id:Int, task: String)
    case class WordCountReply(id:Int,count: Int)
  }

  // Distributed Word counting
  class WordCounterMaster extends Actor {
    import WordCounterMaster._
    override def receive: Receive = {
      case initialize(n) =>
      val childRefs = for(i <- 1 to n)  yield context.actorOf(Props[WordCounterWorker], s"wcw_${i}")
      context.become(withWorkers(childRefs, 0,0, Map()))
    }

    def withWorkers(actorRefs: Seq[ActorRef], currentChild: Int, currentTaskId: Int, requestMap: Map[Int,ActorRef]) : Receive = {
      case task:String =>
        val originalSender = sender()
        println(s"I have received a task with task Id - $currentTaskId, and I am going to assign it to child - $currentChild")
        actorRefs(currentChild) ! WordCountTask(currentTaskId,task)
        val nextChild = (currentChild + 1) % actorRefs.length
        val newTaskId = currentTaskId + 1
        val newRequestMap = requestMap + (currentTaskId -> originalSender)
        context.become(withWorkers(actorRefs,nextChild, newTaskId, newRequestMap))
      case WordCountReply(id, count) =>
        println(s"[master] received a reply with task id - $id with $count")
        requestMap(id) ! count
        context.become(withWorkers(actorRefs, currentChild, currentTaskId,requestMap - id))
    }
  }


  class WordCounterWorker extends Actor {
    import  WordCounterMaster._
    override def receive: Receive = {
      case WordCountTask(id,text) =>
        println(s"${self.path} I have received a task with id - $id and text - $text")
        sender() ! WordCountReply(id,text.split(" ").length)
    }
  }

  class TestActor extends Actor {
    import WordCounterMaster._
    override def receive: Receive = {
      case "Go" =>
       val masterActor =  context.actorOf(Props[WordCounterMaster], "master")
       masterActor ! initialize(2)
       List("I love Vennela!", "I live in Florida!", "I want to live in Canada!").foreach(text => masterActor ! text)

      case count: Integer =>
        println(s"[TestActor] received a reply with count - $count")
    }
  }

  val system = ActorSystem("roundrobinActorSystem")
  val testActor =  system.actorOf(Props[TestActor], "TestActor")

  testActor ! "Go"

}
