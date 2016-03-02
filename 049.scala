val buckets = helpers.Sieve(10000).primesIter(1000).toSeq.groupBy(_.toString.sorted)

val result = for {
	(key, vals) <- buckets
	i <- 0 until vals.size
	if vals(i) != 1487
	j <- i+1 until vals.size
	third = 2*vals(j) - vals(i)
	if vals contains third
} yield "" + vals(i) + vals(j) + third

println(result.head)
