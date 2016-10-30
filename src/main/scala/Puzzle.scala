import scala.collection.mutable.Map

class Puzzle(val title: String, val board: Array[Array[Int]]) {
  var unsolvedSquares: Map[(Int, Int), Array[Int]] = Map.empty
  var madeProgress = true

  def createUnsolvedSquares = {
    for (row <- 0 to 8) {
      for (col <- 0 to 8) {
        if (board(row)(col) == 0) {
          val coord = (row, col)
          var possibilities = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
          for (colCoord <- otherColCoords(coord)) {
            val value = board(coord._1)(colCoord)
            if (possibilities.contains(value)) {
              possibilities = possibilities.filter(_!=value)
            }
          }
          for (rowCoord <- otherRowCoords(coord)) {
            val value = board(rowCoord)(coord._2)
            if (possibilities.contains(value)) {
              possibilities = possibilities.filter(_!=value)
            }
          }
          for (cellCoord <- otherCellCoords(coord)) {
            val value = board(cellCoord._1)(cellCoord._2)
            if (possibilities.contains(value)) {
              possibilities = possibilities.filter(_!=value)
            }
          }
          unsolvedSquares += ((row, col) -> possibilities)
        }
      }
    }
  }

  def removeSquarePossibilities(coord: (Int, Int), value: List[Int]) = {
    def helper(value: List[Int]): Unit = {
      unsolvedSquares(coord) = unsolvedSquares(coord).filter(_ != value.head)
      if (!value.tail.isEmpty) helper(value.tail)
    }
    helper(value)
  }

  def updateSquare(coord: (Int, Int), value: Int): Unit = {
    board(coord._1)(coord._2) = value
    unsolvedSquares -= coord
    checkNearSquares(coord,value)
  }

  def checkNearSquares(coord: (Int,Int), value: Int): Unit = {
    for (colCoord <- otherColCoords(coord)) {
      val checkingCoord = (coord._1,colCoord)
      if(unsolvedSquares.contains(checkingCoord)){
        removeSquarePossibilities(checkingCoord, List(value))
        if(unsolvedSquares(checkingCoord).length == 1) {
          updateSquare(checkingCoord, unsolvedSquares(coord)(0))
        }
      }
    }
    for (rowCoord <- otherRowCoords(coord)) {
      val checkingCoord = (rowCoord, coord._2)
      if(unsolvedSquares.contains(checkingCoord)){
        removeSquarePossibilities(checkingCoord, List(value))
        if(unsolvedSquares(checkingCoord).length == 1) {
          updateSquare(checkingCoord, unsolvedSquares(coord)(0))
        }
      }
    }
    for (cellCoord <- otherCellCoords(coord)) {
      if(unsolvedSquares.contains(cellCoord)){
        removeSquarePossibilities(cellCoord, List(value))
        if(unsolvedSquares(cellCoord).length == 1) {
          updateSquare(cellCoord, unsolvedSquares(coord)(0))
        }
      }
    }
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

  createUnsolvedSquares
}
