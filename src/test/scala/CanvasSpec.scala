import org.scalatest.{FlatSpec, Matchers}

class CanvasSpec extends FlatSpec with Matchers {
  ".apply" should "return a new drawing object with given width and height" in {
    Canvas(3, 3).drawing should fullyMatch regex "\\s{9}"
  }

  it should "keep the drawing when provided" in {
    Canvas(3, 3, "xxxxxxxxx").drawing should fullyMatch regex "x{9}"
  }

  ".fill" should "return a new canvas with the given coordinate filled" in {
    Canvas(3, 3).fill(1, 1, "x").drawing shouldBe "    x    "
  }

  ".render" should "return a bordered canvas with the drawing" in {
    Canvas(20, 4).render shouldBe
      """
        #+--------------------+
        #|                    |
        #|                    |
        #|                    |
        #|                    |
        #+--------------------+
      """.stripMargin('#').trim()
  }

  ".line" should "draw a line into the canvas" in {
    val c = Canvas(20, 4).line(0, 1, 5, 1)
    println(c.render)

    c.render shouldBe
      """
        #+--------------------+
        #|                    |
        #|xxxxxx              |
        #|                    |
        #|                    |
        #+--------------------+
      """.stripMargin('#').trim()
  }
}
