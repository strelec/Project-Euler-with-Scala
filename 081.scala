val matrix = io.Source.fromURL("https://projecteuler.net/project/resources/p081_matrix.txt").getLines.map(
	line => line.split(",").map(_.toInt)
).toVector

val table = Array.fill(matrix.size, matrix.head.size)(0)
table(0)(0) = matrix(0)(0)
(1 until matrix.size).foreach { i =>
	table(i)(0) = table(i-1)(0) + matrix(i)(0)
}
(1 until matrix.head.size).foreach { j =>
	table(0)(j) = table(0)(j-1) + matrix(0)(j)
}

for {
	i <- 1 until matrix.size
	j <- 1 until matrix.head.size
} table(i)(j) = (table(i-1)(j) min table(i)(j-1)) + matrix(i)(j)

val result = table.last.last
println(result)
