import scala.io.Source

object Translator {

  def createPuzzles(file: String) = {
    def helper(split: (List[String], List[String]), accumulator: List[Puzzle] = List.empty): List[Puzzle] = {
      def boardMaker(lines: List[String], board: Array[Array[Int]] = Array.empty): Array[Array[Int]] = {
        var row: Array[Int] = Array.empty
        for (number <- lines.head) {
          row = row :+ number.toString.toInt
        }
        if (lines.tail.isEmpty) {
          return board :+ row
        } else {
          boardMaker(lines.tail, board :+ row)
        }
      }

      val puzzle = new Puzzle(split._1.head, boardMaker(split._1.tail))
      val newAccumulator = accumulator :+ puzzle
      if (split._2.isEmpty) {
        return newAccumulator
      } else {
        helper(split._2.splitAt(10), newAccumulator)
      }
    }

    helper(Source.fromFile(file).getLines().toList.splitAt(10))
  }

}
