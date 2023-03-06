package Part1

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

object MultithreadingRecap extends App {

  class  BankAccount(@volatile private var amount: Int){
  }

  val future = Future {
    42
  }
  future.map(_ + 1)
  future.flatMap(value =>  Future(value+2))

  future onComplete( {
    case Success(42) => println("I found the meaning of life")
    case Failure(exception) => println("something happened with the meaning of life!")
  })

val valueListFC =  for {
    a <- List(1,2,3)
    b <- List('a', 'b', 'c')
    c <- List('a')
  } yield a + "_" +  c

  print(valueListFC)

val valueList =   List(1,2,3).flatMap(num =>  List('a', 'b', 'c').flatMap(char => List('a').map(char =>  num + "_" + char)))

print(valueList)

}
