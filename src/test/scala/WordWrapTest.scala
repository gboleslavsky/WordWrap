import org.scalatest._
import FormatTextForScreen.WordWrap

  class WordWrapTest extends FlatSpec {
    "Line width (argument to lines function)" should "result in a lists of words(separated by whitespace) with overall length with spaces no more than line width" in {
      assert(WordWrap("1 2 3 4 5 6 7 8 9 ").lines(1)
        ===
        List(List("1"), List("2"), List("3"), List("4"), List("5"), List("6"), List("7"), List("8"), List("9")))

      assert(WordWrap("123 456 789").lines(1)
        ===
        List(List("123"), List("456"), List("789")))

      assert(WordWrap("123 456 789").lines(4)
        ===
        List(List("123"), List("456"), List("789")))

      assert(WordWrap("123 456 789").lines(7)
        ===
        List(List("123", "456"), List("789")))

      assert(WordWrap("123 456 789 101112 1314151617 1819202122232425262728").lines(10)
        ===
        List(List("123", "456"), List("789", "101112"), List("1314151617"), List("1819202122232425262728")))

      assert(WordWrap("123 456 789 101112 1314151617 1819202122232425262728").lines(15)
        ===
        List(List("123", "456", "789"), List("101112"), List("1314151617"), List("1819202122232425262728")))

      assert(WordWrap("123 456 789 101112 1314151617 1819202122232425262728").lines(20)
        ===
        List(List("123", "456", "789", "101112"), List("1314151617"), List("1819202122232425262728")))

      assert(WordWrap("123 456 789 101112 1314151617 1819202122232425262728").lines(30)
        ===
        List(List("123", "456", "789", "101112", "1314151617"), List("1819202122232425262728")))




    }
  }