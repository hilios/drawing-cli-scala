

case class Canvas(width: Int, height: Int, drawing: String) {
  private def index(x: Int, y: Int) = x + y * width

  def fill(x: Int, y: Int, filler: Char): Canvas = {
    val i = index(x, y)
    Canvas(width, height, drawing.substring(0, i) ++ filler.toString ++ drawing.substring(i + 1))
  }

  def fill(xy: (Int, Int), filler: Char): Canvas = fill(xy._1, xy._2, filler)

  def render: String = {
    val border = s"+${"-" * width}+"
    val lines = (0 to height - 1)
      .map(i => drawing.substring(i * width, i * width + width))
      .map(l => s"|$l|")
      .mkString("\n")
    s"$border\n$lines\n$border"
  }

  // TODO: Not allow diagonal curves
  def line(x1: Int, y1: Int, x2: Int, y2: Int): Canvas = {
    val coords = (x1 to x2).flatMap(x => (y1 to y2).map(y => (x,y)))
    coords.foldLeft(this)((canvas, xy) => canvas.fill(xy, Canvas.LINE_CHAR))
  }

  def line(xyxy: (Int, Int, Int, Int)): Canvas = line(xyxy._1, xyxy._2, xyxy._3, xyxy._4)

  def rect(x1: Int, y1: Int, x2: Int, y2: Int): Canvas = {
    val coords = Seq(
      (x1, y1, x2, y1),
      (x2, y1, x2, y2),
      (x1, y2, x2, y2),
      (x1, y1, x1, y2)
    )
    coords.foldLeft(this)((canvas, xyxy) => canvas.line(xyxy))
  }

  def bucket(x: Int, y: Int, color: Char): Canvas = {
    val i = index(x, y)
    if(drawing.charAt(i) == Canvas.BLANK_CHAR) {
      val coords = Seq(
        (x, y - 1),
        (x + 1, y),
        (x, y + 1),
        (x - 1, y)
      )
      coords
        .filter(xy => xy._1 >= 0 && xy._1 < width && xy._2 >= 0 && xy._2 < height)
        .foldLeft(fill(x, y, color))((canvas, xy) => canvas.bucket(xy, color))
    } else {
      this
    }
  }

  def bucket(xy: (Int, Int), color: Char): Canvas = bucket(xy._1, xy._2, color)
}

object Canvas {
  val BLANK_CHAR = ' '
  val LINE_CHAR = 'x'

  def apply(width: Int, height: Int) = new Canvas(width, height, BLANK_CHAR.toString * (width * height))
}
