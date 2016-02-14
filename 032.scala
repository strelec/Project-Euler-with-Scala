val result = for {
	i <- 1 to 9876
	j <- i to 10000/i
	prod = i*j
	
	str = i.toString + j.toString + prod.toString
	if str.size == 9
	set = str.toSet
	if !set('0') && set.size == 9
} yield (i, j, prod)

println(result.map(_._3).distinct.sum)
