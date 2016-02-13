val result = (3 to 1000).map( a =>
	(a-1)/2 * a * 2
)

println(result.sum)
