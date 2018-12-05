import com.github.nscala_time.time.Imports._

import scala.io.Source
import scala.util.matching.Regex
import collection.mutable

object Day4 {

  def get_action(text: String): Int = text match {
      case t if t.contains("falls") => 0
      case t if t.contains("wakes") => 1
      case _ => 2
  }

  def parse(text: String):(Int, Int, Period, String) = {
      //[1518-04-12 00:36] falls asleep

      val regex = new Regex(".+#(\\d+).+", "id")
      val result = regex.findAllIn(text)
      val firstMatch = regex.findFirstMatchIn(text)

      val month = text.slice(6,8).toInt
      val day = text.slice(9,11).toInt
      val hours = text.slice(13, 14).toInt
      val minutes = text.slice(15,17).toInt

      val period = 1518.year + month.month + day.day + hours.hours +  minutes.minutes
      val action = get_action(text.slice(18, text.length - 1))
      val id = if (firstMatch.isDefined) result.group("id").toInt else -1

    (id, action, period, text)
  }

  val values = Source.fromFile("C:\\Projects\\advent-of-code\\inputs\\4.txt")
      .getLines
      .map(parse)
      .toList
      .sortBy(l => DateTime.now().plus(l._3)) // thank christ

  def is_sleeping(x: Int): Boolean = x == 0

  def is_awake(x: Int): Boolean = x == 1

  def is_new(x : Int): Boolean = x == 2

  var current_id = -1
  var sleep_time = -1

  val most_asleep = new mutable.HashMap[Int, Int]{ override def default(key:Int) = 0 }
  val most_minutes = new mutable.HashMap[Int, Array[Int]]

  for ((id, action, period, text) <- values){

    println(text)

    if (is_new(action)){
      current_id = id
    }

    if (is_sleeping(action)){
      sleep_time = period.minutes
    }

    if (is_awake(action)){

      val wake_time = period.minutes

      most_asleep(current_id) += wake_time - sleep_time

      if (!most_minutes.contains(current_id)){
        most_minutes(current_id) = new Array[Int](60)
      }

      for(i <- sleep_time until wake_time){
        most_minutes(current_id)(i) += 1
      }
    }
  }

  val best_id = most_asleep.toList.maxBy(_._2)._1

  val time_array = most_minutes(best_id)

  val best_time = time_array.indexOf(time_array.max)

  println(best_id)
  println(best_time)
  println(best_id * best_time)

  val (id, times) = most_minutes.toList.maxBy(_._2.max)

  val max_time = times.indexOf(times.max)

  println(id * max_time)
}
