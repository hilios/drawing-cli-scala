package drawing

import org.scalatest.{FlatSpec, Matchers}

class CanvasSpec extends FlatSpec with Matchers {
  ".apply" should "return a new drawing object with given width and height" in {
    Canvas(3, 3).drawing should fullyMatch regex "\\s{9}"
  }

  it should "keep the drawing when provided" in {
    Canvas(3, 3, "xxxxxxxxx").drawing should fullyMatch regex "x{9}"
  }

  ".fill" should "return a new canvas with the given coordinate filled" in {
    Canvas(3, 3).fill(1, 1, 'x').drawing shouldBe "    x    "
  }

  it should "constraint to the canvas boundaries" in {
    Canvas(3, 3).fill(-1, -1, 'x').drawing shouldBe "x        "
    Canvas(3, 3).fill(10, 10, 'x').drawing shouldBe "        x"
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

  it should "not draw a diagonal line" in {
    an [IllegalArgumentException] should be thrownBy Canvas(20, 4).line(1, 2, 3, 4)
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

  ".bucket" should "fill some area" in {
    Canvas(20, 4)
      .line(0, 1, 5, 1)
      .line(5, 2, 5, 3)
      .rect(15, 0, 19, 2)
      .bucket(9, 2, 'o')
      .render shouldBe
      """
        #+--------------------+
        #|oooooooooooooooxxxxx|
        #|xxxxxxooooooooox   x|
        #|     xoooooooooxxxxx|
        #|     xoooooooooooooo|
        #+--------------------+
      """.stripMargin('#').trim()
  }

  it should "fill random rectangles" in {
    Canvas(20, 4)
      .line(0, 1, 5, 1)
      .line(5, 2, 5, 3)
      .rect(15, 0, 19, 2)
      .bucket(0, 3, '*')
      .bucket(17, 1, '@')
      .render shouldBe
      """
        #+--------------------+
        #|               xxxxx|
        #|xxxxxx         x@@@x|
        #|*****x         xxxxx|
        #|*****x              |
        #+--------------------+
      """.stripMargin('#').trim()
  }
}
