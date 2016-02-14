val (min, i, j) = (for {
	i <- (1L to 2000L).iterator
	j <- (1L to 2000L).iterator
	rectangles = math.abs(i*(i+1)*j*(j+1) - 8000000)
} yield (rectangles, i, j)).min

println(i*j)