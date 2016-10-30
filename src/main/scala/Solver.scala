class Solver(puzzle: Puzzle) {

  def solve = {
    def unsolvedSquaresIterator(solver: Tuple2[Int, Int] => Unit) = {
      for (coordAndPossibilities <- puzzle.unsolvedSquares) {
        solver(coordAndPossibilities._1)
      }
    }

    var solved = false
    var iterations = 0

    def standardSolve = {
      while (puzzle.madeProgress == true && solved == false) {
        puzzle.madeProgress = false
        unsolvedSquaresIterator(rowSolve)
        unsolvedSquaresIterator(colSolve)
        unsolvedSquaresIterator(cellSolve)
        unsolvedSquaresIterator(rowCompareSolve)
        unsolvedSquaresIterator(colCompareSolve)
        unsolvedSquaresIterator(cellCompareSolve)
        if (puzzle.unsolvedSquares.isEmpty) {
          solved = true
        }
        iterations += 1
        if (iterations % 10 == 0) {
          println(puzzle.title + " iterations: " + iterations)
        }
      }
    }

    //if you place alignedNumberSolve into the standardSolve function it causes only 42 to be solved - this probably
    // means that there is an error in the alignedNumberSolve method causing a missed placed number to make a grid unsolvable.

    standardSolve
    alignedNumberSolve
    standardSolve

    if (puzzle.madeProgress == false) {
      println("Cannot solve puzzle: " + puzzle.title)
    }
    println(puzzle.title + " total iterations: " + iterations)
  }

  def otherCellCoords(coord: (Int, Int)) = {
    var cellCoords = Array.tabulate(3, 3)((a, b) => (a, b)).flatten
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

  def compareHelper(value: Int, otherCoords: Array[(Int, Int)], contained: Boolean = false): Boolean = {
    if (otherCoords.isEmpty || contained == true) {
      return contained
    } else {
      compareHelper(value, otherCoords.tail, puzzle.unsolvedSquares(otherCoords.head).contains(value))
    }
  }

  def rowCompareSolve(coord: (Int, Int)): Unit = {
    rowSolve(coord)
    if (puzzle.unsolvedSquares.contains(coord)) {
      for (possibility <- puzzle.unsolvedSquares(coord)) {
        val otherCoords = otherColCoords(coord).map(b => (coord._1, b)).filter(puzzle.unsolvedSquares.contains(_))
        if (!compareHelper(possibility, otherCoords)) {
          puzzle.updateSquare(coord, possibility)
          puzzle.madeProgress = true
        }
      }
    }
  }

  def colCompareSolve(coord: (Int, Int)): Unit = {
    colSolve(coord)
    if (puzzle.unsolvedSquares.contains(coord)) {
      for (possibility <- puzzle.unsolvedSquares(coord)) {
        val otherCoords = otherRowCoords(coord).map(a => (a, coord._2)).filter(puzzle.unsolvedSquares.contains(_))
        if (!compareHelper(possibility, otherCoords)) {
          puzzle.updateSquare(coord, possibility)
          puzzle.madeProgress = true
        }
      }
    }
  }

  def cellCompareSolve(coord: (Int, Int)): Unit = {
    cellSolve(coord)
    if (puzzle.unsolvedSquares.contains(coord)) {
      for (possibility <- puzzle.unsolvedSquares(coord)) {
        val otherCoords = otherCellCoords(coord).filter(puzzle.unsolvedSquares.contains(_))
        if (!compareHelper(possibility, otherCoords)) {
          puzzle.updateSquare(coord, possibility)
          puzzle.madeProgress = true
        }
      }
    }
  }

  def alignedNumberSolve: Unit = {
    val cornerCoords = Array.tabulate(3, 3)((a, b) => (a * 3, b * 3)).flatten
    for (cornerCoord <- cornerCoords) {
      val cellCoords = cornerCoord +: otherCellCoords(cornerCoord)

      def squareCheck(coords: Array[(Int, Int)], value: Int, bool: Boolean = false): Boolean = {
        if (coords.isEmpty) {
          return bool
        }
        if (puzzle.unsolvedSquares(coords.head).contains(value)) {
          return true
        } else {
          squareCheck(coords.tail, value, bool)
        }
      }

      def rowCheck(row: Int) = {

        def rowReduce(value: Int, rowCoords: Array[(Int, Int)]) = {
          //flag made progress in here?
          def helper(coords: Array[(Int, Int)], value: Int): Unit = {
            if (!coords.isEmpty) {
              if (puzzle.unsolvedSquares.contains(coords.head) && puzzle.unsolvedSquares(coords.head).contains(value)) {
                puzzle.removeSquarePossibilities(coords.head, List(value))
                puzzle.madeProgress = true
              }
              helper(coords.tail, value)
            }
          }

          val otherSquares = cellCoords.filter(x => x._1 % 3 != row).filter(puzzle.unsolvedSquares.contains(_))
          if (!squareCheck(otherSquares, value)) {
            //filter correct?
            val rowSquaresWithoutCellSquares = otherColCoords(rowCoords.head).map(b => (rowCoords.head._1, b)).filter(!rowCoords.contains(_))
            helper(rowSquaresWithoutCellSquares, value)
          }
        }

        val rowCoords = cellCoords.filter(x => x._1 % 3 == row).filter(puzzle.unsolvedSquares.contains(_)) //filter correct?
        if (rowCoords.length == 3) {
          for (possibility <- puzzle.unsolvedSquares(rowCoords.head)) {
            if (puzzle.unsolvedSquares(rowCoords(1)).contains(possibility) && puzzle.unsolvedSquares(rowCoords.last).contains(possibility)) {
              rowReduce(possibility, rowCoords)
            }
          }

          for (possibility <- puzzle.unsolvedSquares(rowCoords.head)) {
            if (puzzle.unsolvedSquares(rowCoords(1)).contains(possibility) && !puzzle.unsolvedSquares(rowCoords.last).contains(possibility)) {
              rowReduce(possibility, rowCoords)
            }
            if (!puzzle.unsolvedSquares(rowCoords(1)).contains(possibility) && puzzle.unsolvedSquares(rowCoords.last).contains(possibility)) {
              rowReduce(possibility, rowCoords)
            }
          }
          for (possibility <- puzzle.unsolvedSquares(rowCoords(1))) {
            if (!puzzle.unsolvedSquares(rowCoords.head).contains(possibility) && puzzle.unsolvedSquares(rowCoords.last).contains(possibility)) {
              rowReduce(possibility, rowCoords)
            }
          }
        }
        if (rowCoords.length == 2) {
          for (possibility <- puzzle.unsolvedSquares(rowCoords.head)) {
            if (puzzle.unsolvedSquares(rowCoords.last).contains(possibility)) {
              rowReduce(possibility, rowCoords)
            }
          }
        }
      }

      def colCheck(col: Int) = {

        def colReduce(value: Int, colCoords: Array[(Int, Int)]) = {
          //flag made progress in here?
          def helper(coords: Array[(Int, Int)], value: Int): Unit = {
            if (!coords.isEmpty) {
              if (puzzle.unsolvedSquares.contains(coords.head) && puzzle.unsolvedSquares(coords.head).contains(value)) {
                puzzle.removeSquarePossibilities(coords.head, List(value))
                puzzle.madeProgress = true
              }
              helper(coords.tail, value)
            }
          }

          val otherSquares = cellCoords.filter(x => x._2 % 3 != col).filter(puzzle.unsolvedSquares.contains(_))
          if (!squareCheck(otherSquares, value)) {
            //filter correct?
            val rowSquaresWithoutCellSquares = otherColCoords(colCoords.head).map(a => (a, colCoords.head._2)).filter(!colCoords.contains(_))
            helper(rowSquaresWithoutCellSquares, value)
          }
        }

        val colCoords = cellCoords.filter(x => x._2 % 3 == col).filter(puzzle.unsolvedSquares.contains(_)) //filter correct?
        if (colCoords.length == 3) {
          for (possibility <- puzzle.unsolvedSquares(colCoords.head)) {
            if (puzzle.unsolvedSquares(colCoords(1)).contains(possibility) && puzzle.unsolvedSquares(colCoords.last).contains(possibility)) {
              colReduce(possibility, colCoords)
            }
          }

          for (possibility <- puzzle.unsolvedSquares(colCoords.head)) {
            if (puzzle.unsolvedSquares(colCoords(1)).contains(possibility) && !puzzle.unsolvedSquares(colCoords.last).contains(possibility)) {
              colReduce(possibility, colCoords)
            }
            if (!puzzle.unsolvedSquares(colCoords(1)).contains(possibility) && puzzle.unsolvedSquares(colCoords.last).contains(possibility)) {
              colReduce(possibility, colCoords)
            }
          }
          for (possibility <- puzzle.unsolvedSquares(colCoords(1))) {
            if (!puzzle.unsolvedSquares(colCoords.head).contains(possibility) && puzzle.unsolvedSquares(colCoords.last).contains(possibility)) {
              colReduce(possibility, colCoords)
            }
          }
        }
        if (colCoords.length == 2) {
          for (possibility <- puzzle.unsolvedSquares(colCoords.head)) {
            if (puzzle.unsolvedSquares(colCoords.last).contains(possibility)) {
              colReduce(possibility, colCoords)
            }
          }
        }
      }

      for (num <- 0 to 2) {
        rowCheck(num)
        colCheck(num)
      }
    }
  }


}
