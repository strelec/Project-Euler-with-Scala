val stream = for {
	i <- Stream.from(2)
	j <- 1 to 50

	base = Seq.fill(j)(i.toLong).product
	if (base > 10)
	
	sum = base.toString.map(_.asDigit).sum
	if (i == sum)
} yield (base, i, j)

val result = stream.take(40).sortBy(_._1)

println(result(29)._1)
