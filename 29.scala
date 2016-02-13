val result = (for {
	a <- 2 to 100
	b <- 2 to 100
} yield math.pow(a, b)).toSet.size

println(result)
