import org.scalatest.{Matchers, FlatSpec}

class SolverSpec extends FlatSpec with Matchers {

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
  val solution = Array(
    Array(4, 8, 3, 9, 2, 1, 6, 5, 7),
    Array(9, 6, 7, 3, 4, 5, 8, 2, 1),
    Array(2, 5, 1, 8, 7, 6, 4, 9, 3),
    Array(5, 4, 8, 1, 3, 2, 9, 7, 6),
    Array(7, 2, 9, 5, 6, 4, 1, 3, 8),
    Array(1, 3, 6, 7, 9, 8, 2, 4, 5),
    Array(3, 7, 2, 6, 8, 9, 5, 1, 4),
    Array(8, 1, 4, 2, 5, 3, 7, 6, 9),
    Array(6, 9, 5, 4, 1, 7, 3, 8, 2)
  )
  val solver = new Solver(puzzle)
  val emptyPuzzle = new Puzzle("empty puzzle", Array(
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0)))
  val emptySolver = new Solver(emptyPuzzle)

  "otherCellCoords" should "return the coords of other squares in the same cell for (0,0)" in {
    solver.otherCellCoords((0, 0)) shouldBe Array((0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2))
  }
  it should "return the coords of other squares in the same cell for (5,1)" in {
    solver.otherCellCoords((5, 1)) shouldBe Array((3, 0), (3, 1), (3, 2), (4, 0), (4, 1), (4, 2), (5, 0), (5, 2))
  }

  "otherRowCoords" should "return the column coord of other squares in the same row for (0,0)" in {
    solver.otherRowCoords((0, 0)) shouldBe Array(1, 2, 3, 4, 5, 6, 7, 8)
  }
  it should "return the column coord of other squares in the same row for (5,4)" in {
    solver.otherRowCoords((5, 4)) shouldBe Array(0, 1, 2, 3, 4, 6, 7, 8)
  }

  "otherColCoords" should "return the row coord of other squares in the same row for (0,0)" in {
    solver.otherColCoords((0, 0)) shouldBe Array(1, 2, 3, 4, 5, 6, 7, 8)
  }
  it should "return the row coord of other squares in the same row for (5,4)" in {
    solver.otherColCoords((5, 4)) shouldBe Array(0, 1, 2, 3, 5, 6, 7, 8)
  }

  "rowSolve" should "reduce the possible values of a given square depending on the other squares in the row and flag madeProgress" in {
    solver.rowSolve((0, 0))
    puzzle.unsolvedSquares((0, 0)) shouldBe Array(1, 4, 5, 7, 8, 9)
    puzzle.madeProgress shouldBe true
  }

  "colSolve" should "reduce the possible values of a given square depending on the other squares in the column and flag madeProgress" in {
    solver.colSolve((0, 0))
    puzzle.unsolvedSquares((0, 0)) shouldBe Array(1, 4, 5)
    puzzle.madeProgress shouldBe true
  }

  "cellSolve" should "reduce the possible values of a given square depending on the other squares in the cell and flag madeProgress" in {
    solver.cellSolve((0, 0))
    puzzle.unsolvedSquares((0, 0)) shouldBe Array(4, 5)
    puzzle.madeProgress shouldBe true
  }

  "solve" should "print a message if it cannot solve the puzzle" in {
    emptySolver.solve
    emptyPuzzle.unsolvedSquares.isEmpty shouldBe false
  }
  it should "solve a sudoku puzzle" in {
    solver.solve
    puzzle.board shouldBe solution
    puzzle.unsolvedSquares.isEmpty shouldBe true
  }


}