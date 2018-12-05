import scala.io.Source

object Day2 extends App{

  def find_duplicates(s : String, map: Map[Char, Int] = Map.empty): Set[Int] = s match {
      case "" => map.values.toSet
      case v => find_duplicates(v.tail, map + (v.head -> (map.getOrElse(v.head, 0) + 1)))
  }

  def get_count(s : String): (Int, Int) ={

    val duplicates = find_duplicates(s)

    def bool_to_int(b: Boolean) : Int = {
      if (b) 1 else 0
    }

    def contains(i : Int) : Int = {
      bool_to_int(duplicates.contains(i))
    }

    (contains(2), contains(3))
  }

  def most_similar(values : Stream[String]):String ={

    def max(s1 : String, s2: String):String = {
      if (s1.length >= s2.length) s1 else s2
    }

    def diff(s1: String, s2: String) : String = {
      s1.zip(s2).filter(x => x._1 == x._2).map(_._1).mkString("")
    }

    def min_diff(v : String, values : Stream[String], most_common : String = "") : String = values match {
        case Stream.Empty => most_common
        case head #:: tail => min_diff(v, tail, max(diff(v, head), most_common))
    }

    def common_text(values : Stream[String], common: String = "") : String = values match{
        case Stream.Empty => common
        case head #:: tail => common_text(tail, max(min_diff(head, tail), common))
    }

    common_text(values)
  }

  val lines = Source.fromFile("c:\\projects\\advent-of-code\\inputs\\2.txt").getLines.toStream

  val values = lines
      .map(get_count)
      .reduce((c, p) => (c._1 + p._1, c._2 + p._2))

  println(values._1 * values._2)

  val common = most_similar(lines)

  println(common)
}
