val result = for {
	i <- 1 to 9876
	str = {
		var str = i.toString
		var j = 1
		while (str.size < 9) {
			j += 1
			str += j * i
		}
		str
	}
	if str.size == 9
	
	set = str.toSet
	if !set('0') && set.size == 9
} yield (i, str.toInt)

println(result.map(_._2).max)
