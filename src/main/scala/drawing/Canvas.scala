package drawing

/**
  * A 2D drawing canvas
  * @param width the canvas width
  * @param height the canvas height
  * @param drawing the current canvas drawing
  */
case class Canvas(width: Int, height: Int, drawing: String) {

  /**
    * Return the given coordinate constrained to the canvas boundaries
    * @param coord the axes coordinate
    * @param max the boundary
    * @return the sanitized coordinate
    */
  private def constrained(coord: Int, max: Int) = {
    if(coord < 0) 0 else if (coord < width) coord else max - 1
  }

  /**
    * Returns the linear index from an 2D coordinate
    * @param x the x coordinate
    * @param y the y coordinate
    * @return the linear index
    */
  private def index(x: Int, y: Int) = {
    constrained(x, width) + constrained(y, height) * width
  }

  /**
    * Returns a optimized for humans version of the drawing.
    * @return the drawing
    */
  def render = {
    val border = s"+${"-" * width}+"
    val lines = (0 to height - 1)
      .map(i => drawing.substring(i * width, i * width + width))
      .map(l => s"|$l|")
      .mkString("\n")
    s"$border\n$lines\n$border"
  }

  /**
    * Return a new canvas with the given coordinate filled with the filler parameter
    * @param x the x coordinate
    * @param y the y coordinate
    * @param filler the filler character
    * @return a canvas with a new drawing
    */
  def fill(x: Int, y: Int, filler: Char): Canvas = {
    val i = index(x, y)
    Canvas(width, height, drawing.substring(0, i) ++ filler.toString ++ drawing.substring(i + 1))
  }

  def fill(xy: (Int, Int), filler: Char): Canvas = fill(xy._1, xy._2, filler)

  /**
    * Draws horizontals and vertical lines to the canvas. Throws an exception if one tries to draw a
    * diagonal line
    * @param x1 the stating point x coordinate
    * @param y1 the stating point y coordinate
    * @param x2 the ending point x coordinate
    * @param y2 the ending point y coordinate
    * @return a canvas with a line drawn
    */
  def line(x1: Int, y1: Int, x2: Int, y2: Int): Canvas = {
    val xs = if(x1 < x2) (x1 to x2) else (x2 to x1)
    val ys = if(y1 < y2) (y1 to y2) else (y2 to y1)

    if (xs.length > 1 && ys.length > 1) {
      throw new IllegalArgumentException
    }

    val coords = xs.flatMap(x => ys.map(y => (x,y)))
    coords.foldLeft(this)((canvas, xy) => canvas.fill(xy, Canvas.LINE_CHAR))
  }

  def line(xyxy: (Int, Int, Int, Int)): Canvas = line(xyxy._1, xyxy._2, xyxy._3, xyxy._4)

  /**
    * Draws a rectangle
    * @param x1 the first edge x coordinate
    * @param y1 the first edge y coordinate
    * @param x2 the last edge x coordinate
    * @param y2 the last edge y coordinate
    * @return a canvas with a rectangle drawn
    */
  def rect(x1: Int, y1: Int, x2: Int, y2: Int): Canvas = {
    val coords = Seq(
      (x1, y1, x2, y1),
      (x2, y1, x2, y2),
      (x1, y2, x2, y2),
      (x1, y1, x1, y2)
    )
    coords.foldLeft(this)((canvas, xyxy) => canvas.line(xyxy))
  }

  /**
    * Fills blank areas of the canvas constrained by the edges and lines using a flood-fill
    * algorithm
    * @param x the x coordinate
    * @param y the y coordinate
    * @param color the filler character
    * @return a canvas with a some area filled
    */
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

  /**
    * Returns a new blank canvas
    * @param width the canvas width
    * @param height the canvas height
    * @return the new canvas
    */
  def apply(width: Int, height: Int) =
    new Canvas(width, height, BLANK_CHAR.toString * (width * height))
}
