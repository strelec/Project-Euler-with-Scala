def primes(n: Int) = {
	val nums = 2 to n
	val out = mutable.BitSet(nums: _*)
	for (p <- nums.takeWhile(x => x*x <= n) if out(p))
		out --= p * p to n by p
	out
}

var buckets = Map.empty[Seq[Char], IndexedSeq[Int]].withDefaultValue(IndexedSeq())

primes(9999).dropWhile(_ < 1000).foreach { p =>
	val s = p.toString.toSeq.sorted
	buckets += s -> (buckets(s) :+ p)
}

val solution = buckets.flatMap { case (key, vals) =>
	for {
		i <- 0 until vals.size
		j <- i+1 until vals.size
		third = 2*vals(j) - vals(i)
		if (vals contains third)
	} yield (vals(i), vals(j), third)
}.toSeq.diff(Seq((1487,4817,8147))).head

println("" + solution._1 + solution._2 + solution._3)