val result = for {
	i <- Iterator.from(1)
	if math.log10(i) % 1 < math.log10(10.0/3)
	if (2 to 6).map(_ * i).map(_.toString.sorted).distinct.size == 1
} yield i

println(result.next)
