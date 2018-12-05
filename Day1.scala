import scala.io.Source

object Day1 {

  def add(a : Int,b : Int) = a + b

  def first_duplicate(s: Stream[Int], seen: Set[Int] = Set.empty[Int]): Option[Int] =  s match {
    case head #:: _ if seen(head) => Some(head)
    case head #:: tail => first_duplicate(tail, seen + head)
    case _ => None
  }

  val values = Source.fromFile("c:\\projects\\advent-of-code\\inputs\\1.txt").getLines.map(_.toInt).toStream

  val aggregates = Iterator.continually(values).flatten.scanLeft(0)(add).toStream

  val duplicate = first_duplicate(aggregates)

  val total = values.sum

  println(total)
}
