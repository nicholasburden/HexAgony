package hexagony

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.concurrent.duration.DurationLong

trait Const {
  
  type Move = (Cell, Colour)
  type MaybePath = Option[List[Cell]]
  val O = new Colour(0)
  val R = new Colour(1)
  val B = new Colour(2)
  val K = new Colour(3)
  val RP = new Colour(4)
  val BP = new Colour(5)
  
  def timedRun[T](timelimit: Long)(foo: => T): T = {
    
    val request: Future[T] = Future { foo }
    val duration: Duration = if (timelimit > 0) timelimit millis else Duration.Inf
    Await.result(request, duration)
    
  }
  
}