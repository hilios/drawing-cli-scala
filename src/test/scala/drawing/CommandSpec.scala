package drawing

import org.scalatest.{FlatSpec, Matchers}

class CommandSpec extends FlatSpec with Matchers {
 ".parse" should "return the proper command class with zero-based indexes" in {
   Command.parse("C 20 4") shouldBe CanvasCmd(20, 4)
   Command.parse("L 1 2 3 4") shouldBe LineCmd(0, 1, 2, 3)
   Command.parse("R 1 2 3 4") shouldBe RectCmd(0, 1, 2, 3)
   Command.parse("B 1 2 c") shouldBe BucketCmd(0, 1, 'c')
   Command.parse("Q") shouldBe QuitCmd()
 }

  it should "return invalid command for bad input" in {
    Command.parse("B 1 ccc") shouldBe InvalidCmd()
    Command.parse("CC 20 30") shouldBe InvalidCmd()
    Command.parse("q") shouldBe InvalidCmd()
    Command.parse("Lorem ipsum dolor") shouldBe InvalidCmd()
    Command.parse("R 1 2 3 4 5 6 7") shouldBe InvalidCmd()
    Command.parse("") shouldBe InvalidCmd()
  }
}
