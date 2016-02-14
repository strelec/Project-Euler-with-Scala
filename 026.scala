val result = (1 until 1000).maxBy { n =>
	var map = Map.empty[Int, Int]

	var cur = 10
	var i = 0
	while(!(map isDefinedAt cur)) {
		map += cur -> i
		i += 1

		cur = (cur % n) * 10
	}

	if (map isDefinedAt 0) 0 else i - map(cur)
}

println(result)
