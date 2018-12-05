class Rectangle(point : (Int, Int), w : Int, h:Int) {

  require(point._1 >= 0)
  require(point._2 >= 0)
  require(w > 0)
  require(h > 0)

  val x1 = point._1
  val y1 = point._2

  val width = w
  val height = h

  def y2 = y1 + height - 1
  def x2 = x1 + width - 1

  override def toString: String = f"($x1%s,$y1%s,$width%s,$height%s)"

  def intersects(other: Rectangle): Boolean = {
    !disjoint(other)
  }

  def disjoint(other: Rectangle): Boolean = {
    x1 > other.x2 || x2 < other.x1 || y1 > other.y2 || y2 < other.y1
  }
}