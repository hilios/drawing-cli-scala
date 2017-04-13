import scala.io.StdIn
import scala.util.{Try, Success, Failure}

/**
  * The application entrypoint
  */
object Main extends App {
  val q = """Q""".r
  val c = """C (\d+) (\d+)""".r
  val l = """L (\d+) (\d+) (\d+) (\d+)""".r
  val r = """R (\d+) (\d+) (\d+) (\d+)""".r
  val b = """B (\d+) (\d+) (\w)""".r

  /**
    * Recursively request a command and applies to the canvas
    * @param canvas the current canvas
    */
  def run(canvas: Option[Canvas]): Unit = {
    // Render the canvas if there is any
    canvas.foreach(c => println(c.render))

    val input = StdIn.readLine("enter command: ")
    val command = input match {
      case q() => QuitCmd()
      case c(width, height) => CanvasCmd(width.toInt, height.toInt)
      case l(x1, y1, x2, y2) => LineCmd(x1.toInt - 1, y1.toInt - 1, x2.toInt - 1, y2.toInt - 1)
      case r(x1, y1, x2, y2) => RectCmd(x1.toInt - 1, y1.toInt - 1, x2.toInt - 1, y2.toInt - 1)
      case b(x, y, color) => BucketCmd(x.toInt - 1, y.toInt - 1, color.charAt(0))
      case _ => InvalidCmd()
    }
    command match {
      case cmd: DrawCmd =>
        Try {
          if (cmd.requireCanvas && canvas.isEmpty) {
            println("Please, create a canvas before drawing")
          }
          cmd.draw(canvas)
        } match {
          case Success(d) =>
            run(d)
          case Failure(_) =>
            println("Invalid input")
            run(canvas)
        }
      case _: InvalidCmd =>
        println("Invalid command")
        run(canvas)
      case _: QuitCmd =>
        println("Bye, bye")
    }
  }

  // Start the app
  run(None)
}
