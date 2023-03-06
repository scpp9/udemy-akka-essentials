package Part1

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ThreadModelLimitations extends App {

  val futures = (0 to 9).map(i => i * 100000 until (i + 1)*100000).map(range => Future {
    if(range.contains(546735)) throw new RuntimeException("invalid number")
    range.sum
  })

  Future.reduceLeft(futures)(_ + _).onComplete(println)


}
