package drawing

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

/**
  * The application entrypoint
  */
object Main extends App {

  /**
    * Recursively request a command and applies to the canvas
    * @param canvas the current canvas
    */
  def run(canvas: Option[Canvas]): Unit = {
    // Render the canvas if there is any
    canvas.foreach(c => println(c.render))

    val input = StdIn.readLine("enter command: ")
    Command.parse(input) match {
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
