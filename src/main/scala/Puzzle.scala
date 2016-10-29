import scala.collection.mutable.Map

class Puzzle(val title: String, val board: Array[Array[Int]]) {
  var unsolvedSquares: Map[(Int, Int), Array[Int]] = Map.empty
  var madeProgress = true

  def createUnsolvedSquares = {
    for (row <- 0 to 8) {
      for (col <- 0 to 8) {
        if (board(row)(col) == 0) {
          unsolvedSquares += ((row, col) -> Array(1, 2, 3, 4, 5, 6, 7, 8, 9))
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
    if (unsolvedSquares(coord).length == 1) {
      updateSquare(coord, unsolvedSquares(coord)(0))
    }
  }

  def updateSquare(coord: (Int, Int), value: Int) = {
    board(coord._1)(coord._2) = value
    unsolvedSquares -= coord
  }

  createUnsolvedSquares
}
