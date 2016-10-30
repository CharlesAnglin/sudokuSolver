

object main extends App {

  def elapsedTime(start: Long) = {
    val totalTime = System.currentTimeMillis - start
    if (totalTime < 1000) {
      println("Elapsed time: " + totalTime + " ms")
    } else if (totalTime < 60000) {
      println("Elapsed time: " + totalTime / 1000 + " sec, " + (totalTime - (totalTime / 1000) * 1000) + " ms")
    } else {
      println("Elapsed time: " + totalTime / 60000 + " min, " + (totalTime - (totalTime / 60000) * 60000) / 1000 + " sec, " + (totalTime - (totalTime / 1000) * 1000 + " ms"))
    }
  }

  val start = System.currentTimeMillis()

//  val puzzle = new Puzzle("almost complete", Array(
//    Array(4, 8, 3, 9, 2, 1, 6, 5, 7),
//    Array(9, 6, 7, 3, 4, 5, 8, 2, 1),
//    Array(2, 5, 0, 8, 7, 6, 0, 9, 3),
//    Array(5, 4, 8, 1, 3, 2, 9, 7, 6),
//    Array(7, 2, 9, 5, 6, 4, 1, 3, 8),
//    Array(1, 3, 6, 7, 9, 8, 2, 4, 5),
//    Array(3, 7, 2, 6, 8, 9, 5, 1, 4),
//    Array(8, 0, 4, 2, 5, 3, 7, 6, 9),
//    Array(6, 9, 5, 4, 1, 7, 3, 8, 2)
//  ))
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

//  println(puzzle.unsolvedSquares.mkString(" "))
    solver.solve
//  solver.rowSolve((2,2))
//  solver.rowSolve((6,6))
//  solver.rowSolve((7,1))

  println("unsolved: ")
  for(a <- puzzle.unsolvedSquares){
    println(a._1 + " -> " + a._2.mkString(" "))
  }
  println()
  println(puzzle.board.map(_.mkString(" ")).mkString("\n"))


//  val arrayMatrix = Array(
//    Array(0, 2, 3, 4, 5, 6, 7, 8, 9),
//    Array(1, 2, 3, 4, 5, 6, 7, 8, 9),
//    Array(1, 2, 3, 4, 5, 6, 7, 8, 9),
//    Array(1, 2, 3, 4, 5, 6, 7, 8, 9),
//    Array(1, 2, 3, 4, 5, 6, 7, 8, 9),
//    Array(1, 2, 3, 0, 5, 6, 7, 8, 9),
//    Array(1, 2, 3, 4, 5, 6, 7, 8, 9),
//    Array(1, 2, 3, 4, 5, 6, 7, 8, 9),
//    Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
//  )
//
//  val puzzle = new Puzzle(arrayMatrix)
//
//  //  puzzle.updateSquare((0,0),1)
////  puzzle.removeSquarePossibilities((0,0), List(7,9))
//  //
//  //    println(puzzle.board.map(_.mkString(" ")).mkString("\n"))
//  //  for((a,b) <- puzzle.unsolvedSquares){
//  //    println(s"coord $a:")
//  //    b.map(print)
//  //  }
//
//  val solver = new Solver(puzzle)
//  solver.rowSolve((0,0))
//
//  println(puzzle.board.map(_.mkString(" ")).mkString("\n"))
//
//  println("possiblities: " + puzzle.unsolvedSquares((0,0)).mkString(" "))

//  println(solver.otherCellCoords((4,2)).mkString(" "))
//  println(solver.otherRowCoords((4,2)).mkString(" "))
//  println(solver.otherColCoords((4,2)).mkString(" "))




  elapsedTime(start)

}