class Solver(puzzle: Puzzle) {

  def solve = {

//    for (coordAndPossibilities <- puzzle.unsolvedSquares) {
//      rowSolve(coordAndPossibilities._1)
//    }
//    for (coordAndPossibilities <- puzzle.unsolvedSquares) {
//      colSolve(coordAndPossibilities._1)
//    }


        def unsolvedSquaresIterator(solver: Tuple2[Int, Int] => Unit) = {
          for (coordAndPossibilities <- puzzle.unsolvedSquares) {
            solver(coordAndPossibilities._1)
          }
        }

        var solved = false
        while (puzzle.madeProgress == true && solved == false) {
          println(puzzle.title + " ONE ITTERATION")
          puzzle.madeProgress = false
          unsolvedSquaresIterator(rowSolve)
          unsolvedSquaresIterator(colSolve)
          unsolvedSquaresIterator(cellSolve)
          if (puzzle.unsolvedSquares.isEmpty) {
            solved == true
          }
        }
        if (puzzle.madeProgress == false) {
          println("Cannot solve puzzle: " + puzzle.title)
        }
  }

  def otherCellCoords(coord: (Int, Int)) = {
    var cellCoords = Array.tabulate(3, 3)((a, b) => (a, b)).flatten //could be inefficient - replace with for loop?
    cellCoords = cellCoords.filter(_ !=(coord._1 % 3, coord._2 % 3))
    var coordAddition = (0, 0)
    coord._1 match {
      case 0 | 1 | 2 =>
      case 3 | 4 | 5 => coordAddition = (3, 0)
      case 6 | 7 | 8 => coordAddition = (6, 0)
    }
    coord._2 match {
      case 0 | 1 | 2 =>
      case 3 | 4 | 5 => coordAddition = (coordAddition._1, 3)
      case 6 | 7 | 8 => coordAddition = (coordAddition._1, 6)
    }
    cellCoords.map(cell => (cell._1 + coordAddition._1, cell._2 + coordAddition._2))
  }

  def otherRowCoords(coord: (Int, Int)) = (0 to 8).toArray.filter(_ != coord._1)

  def otherColCoords(coord: (Int, Int)) = (0 to 8).toArray.filter(_ != coord._2)

  def rowSolve(coord: (Int, Int)): Unit = {
    for (colCoord <- otherColCoords(coord)) {
      if (puzzle.unsolvedSquares(coord).contains(puzzle.board(coord._1)(colCoord))) {
        puzzle.removeSquarePossibilities(coord, List(puzzle.board(coord._1)(colCoord)))
        puzzle.madeProgress = true
      }
    }
    if (puzzle.unsolvedSquares(coord).length == 1) {
      puzzle.updateSquare(coord, puzzle.unsolvedSquares(coord)(0))
    }
  }

  def colSolve(coord: (Int, Int)): Unit = {
    for (rowCoord <- otherRowCoords(coord)) {
      if (puzzle.unsolvedSquares(coord).contains(puzzle.board(rowCoord)(coord._2))) {
        puzzle.removeSquarePossibilities(coord, List(puzzle.board(rowCoord)(coord._2)))
        puzzle.madeProgress = true
      }
    }
    if (puzzle.unsolvedSquares(coord).length == 1) {
      puzzle.updateSquare(coord, puzzle.unsolvedSquares(coord)(0))
    }
  }

  def cellSolve(coord: (Int, Int)): Unit = {
    for (cellCoord <- otherCellCoords(coord)) {
      if (puzzle.unsolvedSquares(coord).contains(puzzle.board(cellCoord._1)(cellCoord._2))) {
        puzzle.removeSquarePossibilities(coord, List(puzzle.board(cellCoord._1)(cellCoord._2)))
        puzzle.madeProgress = true
      }
    }
    if (puzzle.unsolvedSquares(coord).length == 1) {
      puzzle.updateSquare(coord, puzzle.unsolvedSquares(coord)(0))
    }
  }

}
