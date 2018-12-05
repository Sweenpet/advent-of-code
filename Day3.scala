import scala.io.Source
import scala.util.matching.Regex

object Day3 {

  def parse_input(text: String): (Rectangle, Int) = {
    val regex = new Regex("#(\\d+)\\s+@\\s+(\\d+),(\\d+):\\s+(\\d+)x(\\d+)", "id", "left", "top", "width", "height")
    val result = regex.findAllIn(text)

    val id = result.group("id").toInt
    val left = result.group("left").toInt
    val top = result.group("top").toInt
    val width = result.group("width").toInt
    val height = result.group("height").toInt

    (new Rectangle((left, top), width, height), id)
  }

  def overlapping_square_count(rectangles : Stream[Rectangle]): Array[Array[Int]] = {
    val matrix = Array.ofDim[Int](1000,1000)

    for (rect <- rectangles){
      for (i <- rect.x1 to rect.x2){
        for (j <- rect.y1 to rect.y2){
          matrix(i)(j) += 1
        }
      }
    }

    matrix
  }

  def non_overlapping_claim_id(rectangles: List[(Rectangle, Int)], remainder: List[(Rectangle, Int)] = List.empty): Int = rectangles match {
      case head :: tail if tail.isEmpty => -1
      case head :: tail if remainder.count(x => x._1 != head._1 && head._1.intersects(x._1)) == 0  && remainder.nonEmpty => head._2
      case head :: tail => non_overlapping_claim_id(tail, if (remainder.isEmpty) rectangles else remainder)
  }

  val lines = Source.fromFile("c:\\projects\\advent-of-code\\inputs\\3.txt")
      .getLines
      .map(parse_input)
      .toStream

  val total_overlap = overlapping_square_count(lines.map(_._1))
    .flatten
    .count(x => x >= 2)

  println(total_overlap)

  val unique_claim_id = non_overlapping_claim_id(lines.toList)

  println(unique_claim_id)
}
