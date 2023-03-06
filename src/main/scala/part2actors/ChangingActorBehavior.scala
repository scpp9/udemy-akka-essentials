package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.ActorCapabilities.Counter

object ChangingActorBehavior extends App {

  object FussyKid {
    case object KidAccept

    case object KidReject

    val HAPPY = "happy"
    val SAD = "sad"
  }

  class FussyKid extends Actor {

    import FussyKid._
    import Mom._

    var state = HAPPY

    override def receive: Receive = {

      case Food(VEGETABLES) => state = SAD
      case Food(CHOCOLATE) => state = HAPPY
      case Ask(message) =>
        if (state == HAPPY) sender() ! KidAccept else sender() ! KidReject
    }
  }

  class FussyKidStateless extends Actor {

    import FussyKid._
    import Mom._

    override def receive: Receive = happyReceive

    def happyReceive: Receive = {
      case Food(VEGETABLES) => context.become(sadReceive, false)
      case Food(CHOCOLATE) =>
      case Ask(_) => sender() ! KidAccept
    }

    def sadReceive: Receive = {
      case Food(CHOCOLATE) => context.become(happyReceive)
      case Food(VEGETABLES) =>
      case Ask(_) => sender() ! KidReject

    }


  }


  object Mom {
    case class MomStart(kidRef: ActorRef)

    case class Food(food: String)

    case class Ask(message: String)

    val VEGETABLES = "veggies"
    val CHOCOLATE = "chocolate"
  }

  class Mom extends Actor {

    import FussyKid._
    import Mom._

    override def receive: Receive = {
      case MomStart(kidRef) =>
        kidRef ! Food(VEGETABLES)
        kidRef ! Food(VEGETABLES)
        kidRef ! Food(CHOCOLATE)
        kidRef ! Ask("Do you want to play?")
      case KidAccept => println("Yay, my kid is happy");
      case KidReject => println("My kid is sad, but as he's healthy!")
    }
  }

  val system = ActorSystem("changingActorBehaviorDemo")
  val fussyKid = system.actorOf(Props[FussyKid])
  val mom = system.actorOf(Props[Mom])
  val fussyKidStateless = system.actorOf(Props[FussyKidStateless])

  import Mom._

  mom ! MomStart(fussyKidStateless)


  //   Exercise - 1 - recreate the Counter Actor with context.become and No MUTABLE STATE
  class CounterActor extends Actor {

    import Counter._

    override def receive: Receive = countReceive(0)

    def countReceive(count: Int): Receive = {
      case Increment => {
        println(s"[$count] incrementing")
        context.become(countReceive(count + 1))
      }
      case Decrement => {
        println(s"[$count] decrementing")
        context.become(countReceive(count - 1))
      }
      case Print => println(s"[count] my current count is $count")
    }
  }

  import Counter._

  val counter = system.actorOf(Props[CounterActor], "counterActor")
  (1 to 1) foreach (_ => counter ! Increment)
  (1 to 6) foreach (_ => counter ! Decrement)

  counter ! Print

  /**
   * Excercise 2
   * voting system
   *
   *
   */

  case class Vote(candidate: String)

  case object VoteStatusRequest

  case class VoteStatusReply(candidate: Option[String])



  class Citizen extends Actor {
    override def receive: Receive = {
      case Vote(c) => context.become(voted(Some(c)))
      case VoteStatusRequest =>
        context.become(voted(None))
    }

    def voted(candidate:Option[String]): Receive = {
      case Vote(c) =>
      case VoteStatusRequest => sender() ! VoteStatusReply(candidate)
    }

  }

  case class AggregateVotes(citizens: Set[ActorRef])

  class VoteAggregator extends Actor {

    override def receive: Receive = {
      case AggregateVotes(citizens) => {
         citizens.foreach(citizen => citizen ! VoteStatusRequest)
         context.become(aggregateVotes(citizens, Map()))
      }
    }

    private def aggregateVotes(stillWaiting: Set[ActorRef], voteAggregatorMap: Map[String, Int]) : Receive = {
      case VoteStatusReply(None)  => sender() ! VoteStatusRequest
      case VoteStatusReply(Some(c)) => {
          val newvoteAggregatorMap =  voteAggregatorMap + (c -> (voteAggregatorMap.getOrElse(c, 0) + 1))
          val newStillWaiting = stillWaiting - sender()
          if(newStillWaiting.isEmpty){
            println(newvoteAggregatorMap)
          }else {
            context.become(aggregateVotes(newStillWaiting, newvoteAggregatorMap))
          }
      }
    }

  }


  val alice = system.actorOf(Props[Citizen])
  val bob = system.actorOf(Props[Citizen])
  val charlie = system.actorOf(Props[Citizen])
  val daniel = system.actorOf(Props[Citizen])

  alice ! Vote("Martin")
  bob ! Vote("Jonas")
  charlie  ! Vote("Roland")
  daniel ! Vote("Roland")


  val voteAggregator = system.actorOf(Props[VoteAggregator])

  voteAggregator ! AggregateVotes(Set.apply(alice, bob, charlie, daniel))

}
