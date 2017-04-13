/**
  * Defines a command that draws something to the canvas.
  */
trait DrawCmd {
  val requireCanvas = false

  def draw(canvas: Option[Canvas]): Option[Canvas] = ???
}

/**
  * Defines a command that requires the canvas to draw.
  */
trait RequireCanvas { self: DrawCmd =>
  override val requireCanvas = true
}

/**
  * Draws a new empty canvas
  * @param width the canvas width
  * @param height the canvas height
  */
case class CanvasCmd(width: Int, height: Int) extends DrawCmd {
  override def draw(canvas: Option[Canvas]) = Some(Canvas(width, height))
}

/**
  * Draws a line to the canvas
  * @param x1 the first point x coordinate
  * @param y1 the first point y coordinate
  * @param x2 the last point x coordinate
  * @param y2 the last point y coordinate
  */
case class LineCmd(x1: Int, y1: Int, x2: Int, y2: Int) extends DrawCmd with RequireCanvas {
  override def draw(canvas: Option[Canvas]) = canvas.map(_.line(x1, y1, x2, y2))
}

/**
  * Draws a rectangle to the canvas
  * @param x1 the first edge x coordinate
  * @param y1 the first edge y coordinate
  * @param x2 the last edge x coordinate
  * @param y2 the last edge y coordinate
  */
case class RectCmd(x1: Int, y1: Int, x2: Int, y2: Int) extends DrawCmd with RequireCanvas {
  override def draw(canvas: Option[Canvas]) = canvas.map(_.rect(x1, y1, x2, y2))
}

/**
  * Fill an area with some color
  * @param x the x coordinate
  * @param y the y coordinate
  * @param color the filling color
  */
case class BucketCmd(x: Int, y: Int, color: Char) extends DrawCmd with RequireCanvas {
  override def draw(canvas: Option[Canvas]) = canvas.map(_.bucket(x, y, color))
}

/**
  * Quit the application
  */
case class QuitCmd()

/**
  * An invalid command
  */
case class InvalidCmd()