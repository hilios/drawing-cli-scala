import drawing.Draw
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
}
