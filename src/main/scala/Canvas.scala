

case class Canvas(width: Int, height: Int, drawing: String) {
  private def toIndex(x: Int, y: Int) = x + y * width

  def fill(x: Int, y: Int, filler: String): Canvas = {
    val i = toIndex(x, y)
    Canvas(width, height, drawing.substring(0, i).concat(filler).concat(drawing.substring(i + 1)))
  }
}

object Canvas {
  def apply(width: Int, height: Int): Canvas = new Canvas(width, height, " " * (width * height))
}
