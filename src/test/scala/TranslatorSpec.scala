import org.scalatest.{Matchers, FlatSpec}

class TranslatorSpec extends FlatSpec with Matchers {

  val puzzle = new Puzzle("Grid 00", Array(
    Array(0, 0, 3, 0, 2, 0, 6, 0, 0),
    Array(9, 0, 0, 3, 0, 5, 0, 0, 1),
    Array(0, 0, 1, 8, 0, 6, 4, 0, 0),
    Array(0, 0, 8, 1, 0, 2, 9, 0, 0),
    Array(7, 0, 0, 0, 0, 0, 0, 0, 8),
    Array(0, 0, 6, 7, 0, 8, 2, 0, 0),
    Array(0, 0, 2, 6, 0, 9, 5, 0, 0),
    Array(8, 0, 0, 2, 0, 3, 0, 0, 9),
    Array(0, 0, 5, 0, 1, 0, 3, 0, 0)))

  "createPuzzles" should "return a list of puzzles given a .txt file" in {
    val puzzles = Translator.createPuzzles("test.txt")
    puzzles.length shouldBe 1
    puzzles.head.board shouldBe puzzle.board
    puzzles.head.unsolvedSquares.keys shouldBe puzzle.unsolvedSquares.keys
    //compare sizes as to compare two arrays a and b you need to use a.deep == b.deep
    puzzles.head.unsolvedSquares.values.size shouldBe puzzle.unsolvedSquares.values.size
    puzzles.head.title shouldBe puzzle.title
  }

}
