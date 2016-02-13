val N = 1000000

/*val result = (3 to N / 4 + 1).map { i =>
	val min = math.sqrt(i * i - N).floor.round
	(i - min - 1) / 2
}*/

val result = (3 to N / 4 + 1).map { i =>
	val sq = i*i
	
	var j = i - 2
	var c = 0
	while (j > 0 && sq - j*j <= N) {
		c += 1
		j -= 2
	}
	c
}

println(result.sum)
