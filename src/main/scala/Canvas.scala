

case class Canvas(width: Int, height: Int, drawing: String) {
  private def toIndex(x: Int, y: Int) = x + y * width

  def fill(x: Int, y: Int, filler: String): Canvas = {
    val i = toIndex(x, y)
    Canvas(width, height, drawing.substring(0, i) ++ filler ++ drawing.substring(i + 1))
  }

  def fill(xy: (Int, Int), filler: String): Canvas = fill(xy._1, xy._2, filler)

  def render: String = {
    val border = s"+${"-" * width}+"
    val lines = (0 to height - 1)
      .map(i => drawing.substring(i * width, i * width + width))
      .map(l => s"|$l|")
      .mkString("\n")
    s"$border\n$lines\n$border"
  }

  def line(x1: Int, y1: Int, x2: Int, y2: Int): Canvas = {
    val coords = (x1 to x2).flatMap(x => (y1 to y2).map(y => (x,y)))
    coords.foldLeft(this)((canvas, xy) => canvas.fill(xy, Canvas.LINE_CHAR))
  }
}

object Canvas {
  val LINE_CHAR = "x"
  val BLANK_CHAR = " "

  def apply(width: Int, height: Int) = new Canvas(width, height, BLANK_CHAR * (width * height))
}
