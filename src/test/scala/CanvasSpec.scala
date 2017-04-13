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

  ".line" should "draw a horizontal line into the canvas" in {
    Canvas(20, 4).line(0, 1, 5, 1).render shouldBe
      """
        #+--------------------+
        #|                    |
        #|xxxxxx              |
        #|                    |
        #|                    |
        #+--------------------+
      """.stripMargin('#').trim()
  }

  it should "draw a vertical line into the canvas" in {
    Canvas(20, 4).line(5, 2, 5, 3).render shouldBe
      """
        #+--------------------+
        #|                    |
        #|                    |
        #|     x              |
        #|     x              |
        #+--------------------+
      """.stripMargin('#').trim()
  }

  ".rect" should "draw a rectangle" in {
    Canvas(20, 4).rect(15, 0, 19, 2).render shouldBe
      """
        #+--------------------+
        #|               xxxxx|
        #|               x   x|
        #|               xxxxx|
        #|                    |
        #+--------------------+
      """.stripMargin('#').trim()
  }
}
