import scala.io.Source

object Day5 extends App {

  def is_destructive(a: Char, b: Char): Boolean ={
    math.abs(a.toInt - b.toInt) == 32
  }

  def reduce_polymer(forward : List[Char], back: List[Char] = List.empty):List[Char] = forward match{
    case Nil => back
    case head :: tail if back.nonEmpty && is_destructive(back.head, head) => reduce_polymer(tail, back.tail)
    case head :: tail => reduce_polymer(tail, List(head) ::: back)
  }

  def max_redux(chars: List[(Char, Char)], polymer: List[Char], max: Int = Int.MaxValue):Int = chars match {
    case Nil => max
    case head :: tail => val reduced = reduce_polymer(polymer.filter(a => a != head._1 && a != head._2)); max_redux(tail, polymer, math.min(reduced.length, max))
  }

  val polymer = Source.fromFile("C:\\Projects\\advent-of-code\\inputs\\5.txt").mkString.toList
  val reduced = reduce_polymer(polymer)

  println(reduced.length)

  val chars = List.range(65, 91).map(m => (m.toChar, (m + 32).toChar))
  val max_reduction = max_redux(chars, polymer)

  println(max_reduction)
}
