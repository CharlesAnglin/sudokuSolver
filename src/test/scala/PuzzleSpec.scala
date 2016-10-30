import org.scalatest.{Matchers, FlatSpec}

class PuzzleSpec extends FlatSpec with Matchers {

  val puzzle = new Puzzle("puzzle 1", Array(
    Array(0, 0, 3, 0, 2, 0, 6, 0, 0),
    Array(9, 0, 0, 3, 0, 5, 0, 0, 1),
    Array(0, 0, 1, 8, 0, 6, 4, 0, 0),
    Array(0, 0, 8, 1, 0, 2, 9, 0, 0),
    Array(7, 0, 0, 0, 0, 0, 0, 0, 8),
    Array(0, 0, 6, 7, 0, 8, 2, 0, 0),
    Array(0, 0, 2, 6, 0, 9, 5, 0, 0),
    Array(8, 0, 0, 2, 0, 3, 0, 0, 9),
    Array(0, 0, 5, 0, 1, 0, 3, 0, 0)))
  val solver = new Solver(puzzle)

  "removeSquarePossibilities" should "remove the given values as possibilities for a given square" in {
    puzzle.removeSquarePossibilities((0, 0), List(9, 3, 2))
    puzzle.unsolvedSquares((0, 0)) shouldBe Array(1, 4, 5, 6, 7, 8)
  }

  "updateSquare" should "update the given square on the board and remove the possible values of a square" in {
    puzzle.updateSquare((0, 1), 8)
    puzzle.board(0)(1) shouldBe 8
    puzzle.unsolvedSquares.contains((0, 1)) shouldBe false
  }


}
