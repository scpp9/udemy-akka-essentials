package part2actors


import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.ActorCapabilities.Person.LiveLife


object ActorCapabilities extends App {

  class SimpleActor extends Actor {

    override def receive: Receive = {
      case "Hi!" => sender() ! "Hello There!"
      case message: String => println(s"$self I have received $message")
      case number: Int => println(s"[simple actor] I have received $number")
      case SpecialMessage(contents) => println(s"[simple actor] I have received $contents")
      case SendMessageToYourself(contents) => self ! contents
      case SayHiTo(ref) => ref ! "Hi!"
      case WirelessPhoneMessage(contents, ref) => ref forward contents + "s"
    }

  }

  val system = ActorSystem("actorCapabilitiesDemo")
  val simpleActor = system.actorOf(Props[SimpleActor], "simpleActor")

  simpleActor ! "hello, actor!"
  simpleActor ! 42

  case class SpecialMessage(contents: String)

  simpleActor ! SpecialMessage("Special Message")

  case class SendMessageToYourself(message: String)

  simpleActor ! SendMessageToYourself("I am an actor and I am proud of it!")

  // How Actor can reply to their messages
  val bob = system.actorOf(Props[SimpleActor], "bob")
  val alice = system.actorOf(Props[SimpleActor], name = "alice")

  case class SayHiTo(ref: ActorRef)

  alice ! SayHiTo(bob)

  alice ! "Hi!"


  // 5 - forwarding messages
  // D -> A -> B
  //forwarding = sending a message with original sender
  case class WirelessPhoneMessage(content: String, ref: ActorRef)

  alice ! WirelessPhoneMessage("Hi", bob)

  /*
  *Excercises
  *
  * 1. a Counter actor
  *  - Increment
  *  - Decrement
  *  - Print
  *
  *
   */

  object Counter {
    case object Increment

    case object Decrement

    case object Print
  }

  class CounterActor extends Actor {

    import Counter._

    var count = 0

    override def receive: Receive = {
      case Increment => count += 1
      case Decrement => count -= 1
      case Print => println(s"[counter] My current count is $count")

    }

  }

  import Counter._

  val counter = system.actorOf(Props[CounterActor], "counterActor")
  (1 to 1) foreach (_ => counter ! Increment)
  (1 to 6) foreach (_ => counter ! Decrement)

  counter ! Print

  object BankAccount {
    case object Statement

    case class Deposit(amount: Int)

    case class Withdraw(amount: Int)

    case class TransactionSuccess(message: String)

    case class TransactionFailure(message: String)
  }

  class BankAccountActor extends Actor {

    import BankAccount._

    var funds = 0

    override def receive: Receive = {
      case Deposit(amount) =>
        if (amount < 0) sender() ! TransactionFailure("invalid deposit amount!")
        else {
          funds += amount
          sender() ! TransactionSuccess("amount deposited successfully!")
        }
      case Withdraw(amount) => if (amount < 0)
        sender() ! TransactionFailure("invalid withdraw amount!")
      else if (amount > funds) sender() ! TransactionFailure("Insufficient funds")
      else {
        funds -= amount
        sender() ! TransactionSuccess("amount successfully withdrawn!")
      }
      case Statement => sender() ! s"[funds] Current balance is $funds"
    }
  }
  
  object Person {
    case class LiveLife(ref: ActorRef)
  }

  class Person extends Actor {

    import BankAccount._

    override def receive: Receive = {
      case LiveLife(ref) => {
        ref ! Deposit(1000)
        ref ! Withdraw(10000)
        ref ! Withdraw(1000)
        ref ! Statement
      }
      case msg => println(msg)
    }
  }

  val sri = system.actorOf(Props[Person], "Person")

  val account = system.actorOf(Props[BankAccountActor], "account")

  sri ! LiveLife(account)
}