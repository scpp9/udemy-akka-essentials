package part2actors

import akka.actor.{Actor, ActorSystem, Props}


object ActorsIntro extends App{

  //part1 - actor systems
  private val actorSystem = ActorSystem("firstActorSystem")
  println(actorSystem.name)

  //part2 - create actors
  //word count actor

  class WordCounterActor extends Actor {
    private var totalWords =0

    // behavior
    // behavior
    def receive: Receive = {
      case message: String =>
        println(s"[word counter] I have received: $message")
        totalWords += message.split(" ").length
      case msg => println(s"[word counter] I cannot understand ${msg.toString}")
    }



    
}

  private val wordCounter = actorSystem.actorOf(Props[WordCounterActor], "wordCounter")

  wordCounter ! "x"
  //
//  object Person {
//
//  }
//
//  class Person(name: String) extends Actor {
//    override def receive: Receive = ???
//  }


}
