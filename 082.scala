val matrix = io.Source.fromURL("https://projecteuler.net/project/resources/p082_matrix.txt").getLines.map(
	line => line.split(",").map(_.toInt)
).toVector

val table = Array.fill(matrix.head.size, matrix.size)(0)
(0 until matrix.size).foreach { i =>
	table(0)(i) = matrix(i)(0)
}

for {
	j <- 1 until matrix.head.size
	i <- 0 until matrix.size
} table(j)(i) = (0 until matrix.size).map { prev =>
	val interval = Seq(i, prev).sorted match { case Seq(a, b) => a to b }
	table(j-1)(prev) + interval.map(matrix(_)(j)).sum
}.min

val result = table.last.min
println(result)
