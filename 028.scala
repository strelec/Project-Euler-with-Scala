val n = 1001
val k = n/2 - 1

val result = for {
	i <- (0 to k).toIterator
	main = 4*i*i + 12*i + 9
	m <- 0 to 3
	remove = 2*(i+1)*m
} yield main - remove

println(result.sum + 1)