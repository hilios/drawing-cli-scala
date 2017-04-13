import scala.io.StdIn

object Main extends App {
  val q = "Q".r
  val c = "C (\\d+) (\\d+)".r
  val l = "L (\\d+) (\\d+) (\\d+) (\\d+)".r
  val r = "R (\\d+) (\\d+) (\\d+) (\\d+)".r
  val b = "B (\\d+) (\\d+) (\\w)".r

  def checkCanvas(canvas: Option[Canvas]): Option[Canvas] = {
    if (canvas.isEmpty) {
      println("Please, create a canvas before drawing")
    }
    canvas
  }

  def run(canvas: Option[Canvas]): Unit = {
    // Render the canvas if there is any
    canvas.foreach(c => println(c.render))

    val input = StdIn.readLine("enter command: ")
    input match {
      case q() =>
        println("Bye, bye")
      case c(width, height) =>
        val output = Canvas(width.toInt, height.toInt)
        run(Some(output))
      case l(x1, y1, x2, y2) =>
        val output = checkCanvas(canvas)
          .map(_.line(x1.toInt - 1, y1.toInt - 1, x2.toInt - 1, y2.toInt - 1))
        run(output)
      case r(x1, y1, x2, y2) =>
        val output = checkCanvas(canvas)
          .map(_.rect(x1.toInt - 1, y1.toInt - 1, x2.toInt - 1, y2.toInt - 1))
        run(output)
      case b(x, y, color) =>
        val output = checkCanvas(canvas)
          .map(_.bucket(x.toInt - 1, y.toInt - 1, color.charAt(0)))
        run(output)
      case _ =>
        println("Invalid command")
        run(canvas)
    }
  }

  // Start the app
  run(None)
}
