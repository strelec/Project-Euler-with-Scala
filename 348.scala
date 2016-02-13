val palin = for {
	i <- Iterator.from(1)
	d = Seq.fill(i)(10).product

	fill <- Seq(d/10, d)
	n <- (d/10 to d-1).toIterator
	nrev = n.toString.reverse.toInt
} yield n*fill + nrev%fill

val result = palin.filter( n =>
	(1 to math.cbrt(n).toInt).filter { cb =>
		val rem = n - cb*cb*cb
		var sq = math.sqrt(rem).toInt
		rem == sq*sq
	}.take(5).size == 4
).take(5).toList

println(result.sum)