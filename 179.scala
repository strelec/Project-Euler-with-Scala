val N = 10000000

val table = Array.fill(N)(0)
table(0) = -1
for {
	n <- 2 until N
	add <- n until N by n
} table(add) += 1

val result = table.sliding(2).count( x =>
	x(0) == x(1)
)
println(result)
