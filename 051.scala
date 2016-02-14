import helpers.Sieve

val iter = Sieve(999999).primesIter()

val result = for {
	i <- (1 to 6).toStream
	n = Seq.fill(i)(10).product
} yield {
	var map = Map.empty[Set[(Char, Int)], Seq[Int]].withDefaultValue(Seq())
	for {
		prime <- iter.takeWhile(_ < n)
		places = prime.toString.zipWithIndex

		whole = places.toSet
		available = places.init.toSet

		digit <- '0' to '9'
		toRemove <- available.filter(_._1 == digit).subsets.drop(1)

		choice = whole &~ toRemove
	} map += choice -> (prime +: map(choice))
	map.values.filter(_.size == 8).flatten.toSeq.sorted
}

println(result.flatten.head)