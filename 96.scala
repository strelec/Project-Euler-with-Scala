import sudoku._
import util.Helpers._

// this needs the LVR-scala project

val sudokus = io.Source.fromFile("p096_sudoku.txt").getLines.grouped(10).map( sudoku =>
	Sudoku(sudoku.tail.map( line =>
		line.map {
			case '0' => Prazno
			case x => x.asDigit
		}
	))
).toVector

val result = sudokus.map { sudoku =>
	val rešitev = sudoku.rešitev.s
	val tri = rešitev.head.take(3)
	tri.mkString.toInt
}

println(result.sum)
