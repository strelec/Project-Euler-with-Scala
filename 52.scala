val result = (Stream from 1).filter { i =>
	math.log10(i) % 1 < math.log10(10/3)
}.filter { i =>
	(2 to 6).map(x => (x * i).toString.sorted).distinct.size == 1
}

println(result.head)