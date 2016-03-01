val palin = for {
	d <- helpers.Number.powers(10)
	fill <- Seq(d/10, d)
	n <- (d/10 to d-1).toIterator
	nr = helpers.Number.reverse(n)
} yield n*fill + nr%fill

val result = palin.filter( n =>
	(1 to math.cbrt(n).toInt).filter { cb =>
		val rem = n - cb*cb*cb
		var sq = math.sqrt(rem).toInt
		rem == sq*sq
	}.take(5).size == 4
).take(5).toList

println(result.sum)
