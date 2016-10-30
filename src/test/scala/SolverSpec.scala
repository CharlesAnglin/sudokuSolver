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
  val puzzle2 = new Puzzle("puzzle 2", Array( // puzzle2 is madeup - unsolvable
    Array(0, 0, 3, 0, 2, 0, 6, 0, 0),
    Array(9, 0, 0, 3, 0, 7, 0, 0, 1),
    Array(0, 0, 1, 8, 0, 6, 4, 0, 0),
    Array(0, 0, 8, 1, 0, 2, 9, 0, 0),
    Array(7, 0, 0, 0, 0, 4, 0, 0, 8),    //solve (4,3), reduce (0,5)
    Array(0, 0, 6, 7, 0, 8, 2, 0, 0),
    Array(0, 0, 2, 6, 0, 9, 1, 0, 0),
    Array(8, 0, 0, 2, 0, 3, 0, 0, 9),
    Array(0, 0, 5, 0, 1, 0, 3, 0, 0)))
  val solver2 = new Solver(puzzle2)

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
  it should "update the given square on the board if there is only one possible value left" in {
    puzzle.removeSquarePossibilities((0, 1), List(1, 3, 4, 5, 7, 9))
    solver.rowSolve((0, 1))
    puzzle.board(0)(1) shouldBe 8
    puzzle.unsolvedSquares.contains((0, 1)) shouldBe false
  }

  "colSolve" should "reduce the possible values of a given square depending on the other squares in the column and flag madeProgress" in {
    solver.colSolve((0, 0))
    puzzle.unsolvedSquares((0, 0)) shouldBe Array(1, 4, 5)
    puzzle.madeProgress shouldBe true
  }
  it should "update the given square on the board if there is only one possible value left" in {
    puzzle.removeSquarePossibilities((0, 3), List(4, 5))
    solver.colSolve((0, 3))
    puzzle.board(0)(3) shouldBe 9
    puzzle.unsolvedSquares.contains((0, 3)) shouldBe false
  }

  "cellSolve" should "reduce the possible values of a given square depending on the other squares in the cell and flag madeProgress" in {
    solver.cellSolve((0, 0))
    puzzle.unsolvedSquares((0, 0)) shouldBe Array(4, 5)
    puzzle.madeProgress shouldBe true
  }
  it should "update the given square on the board if there is only one possible value left" in {
    puzzle.removeSquarePossibilities((0, 5), List(4, 7, 9))
    solver.cellSolve((0, 5))
    puzzle.board(0)(5) shouldBe 1
    puzzle.unsolvedSquares.contains((0, 5)) shouldBe false
  }

  "rowCompareSolve" should "update the given square on the board if there is only one possible for it when comparing to the rest of the row" in {
    puzzle.removeSquarePossibilities((1, 2), List(6))
    puzzle.removeSquarePossibilities((1, 4), List(6))
    puzzle.removeSquarePossibilities((1, 6), List(6))
    puzzle.removeSquarePossibilities((1, 7), List(6))
    solver.rowCompareSolve((1, 1))
    puzzle.board(1)(1) shouldBe 6
    puzzle.unsolvedSquares.contains((1, 1)) shouldBe false
  }

  "colCompareSolve" should "update the given square on the board if there is only one possible for it when comparing to the rest of the col" in {
    puzzle.removeSquarePossibilities((2, 8), List(7))
    puzzle.removeSquarePossibilities((3, 8), List(7))
    puzzle.removeSquarePossibilities((5, 8), List(7))
    puzzle.removeSquarePossibilities((6, 8), List(7))
    puzzle.removeSquarePossibilities((8, 8), List(7))
    solver.colCompareSolve((0, 8))
    puzzle.board(0)(8) shouldBe 7
    puzzle.unsolvedSquares.contains((0, 8)) shouldBe false
  }

  "cellCompareSolve" should "update the given square on the board if there is only one possible for it when comparing to the rest of the cell" in {
    puzzle.removeSquarePossibilities((0, 0), List(7))
    puzzle.removeSquarePossibilities((2, 0), List(7))
    puzzle.removeSquarePossibilities((2, 1), List(7))
    solver.cellCompareSolve((1, 2))
    puzzle.board(1)(2) shouldBe 7
    puzzle.unsolvedSquares.contains((1, 2)) shouldBe false
  }

  "alignedNumberSolve" should "reduce the possible values of a square if there exists aligned possible values in another cell and flag madeProgress" in {

  }

  "solve" should "print a message if it cannot solve the puzzle" in {
    val stream = new java.io.ByteArrayOutputStream()
    Console.withOut(stream) {
      emptySolver.solve
    }
    stream.toString.contains("Cannot solve puzzle: empty puzzle") shouldBe true
    emptyPuzzle.unsolvedSquares.isEmpty shouldBe false
  }
  it should "solve a sudoku puzzle and print off the numbers of iterations" in {
    val stream = new java.io.ByteArrayOutputStream()
    Console.withOut(stream) {
      solver.solve
    }
    stream.toString.contains("puzzle 1 total iterations:") shouldBe true
    puzzle.board shouldBe solution
    puzzle.unsolvedSquares.isEmpty shouldBe true
  }

}