package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.ChildActors.Parent.{CreateChild, TellChild}

object ChildActors extends  App {

  object Parent {
    case class CreateChild(name: String)
    case class TellChild(message: String)
  }

  class Parent extends Actor{
    override def receive: Receive = {
      case CreateChild(name) =>
      println(s"${self.path} creating child!")
      context.become(withChild(context.actorOf(Props[Child],name)))
    }

    import Parent._
    def withChild(actorRef: ActorRef): Receive = {
      case TellChild(message) => if(actorRef != null) actorRef ! message
    }
  }

   class  Child extends Actor{
     override def receive: Receive = {
       case message => println(s"${self.path} message is $message")
     }
   }

   val actorSystem = ActorSystem("ParentChildDemo")
   val parent = actorSystem.actorOf(Props[Parent], "Parent")

  parent ! CreateChild("child")
  parent ! TellChild("Hey kid!")


  //Actor Selection


}
