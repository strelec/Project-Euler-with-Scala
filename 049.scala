val buckets = helpers.Sieve(10000).primesIter(1000).toSeq.groupBy(_.toString.sorted)

val result = for {
	(key, vals) <- buckets
	i <- vals.indices
	if vals(i) != 1487
	j <- vals.indices.drop(i+1)
	third = 2*vals(j) - vals(i)
	if vals contains third
} yield "" + vals(i) + vals(j) + third

println(result.head)
