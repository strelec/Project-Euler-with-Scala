val n = 50

val irregular = for {
	ax <- 0 to n
	ay <- 0 to n
	if (ax != 0) || (ay != 0)
	
	bx <- 0 to n
	by <- 0 to n
	if (bx != 0) || (by != 0)
	if (ax != bx) || (ay != by)
	
	if 2*ax*ax + 2*ay*ay - 2*ax*bx - 2*ay*by == 0
} yield ((ax, ay), (bx, by))

val result = irregular.size + n*n

println(result)
