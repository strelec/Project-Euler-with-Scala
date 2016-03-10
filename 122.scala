import collection.BitSet

val N = 200
val memo = Array.fill(N+1)(Seq.empty[BitSet])
memo(1) = Seq(BitSet(1))

val result = (2 to N).map { i =>
	val tmp = for {
		j <- 1 to i/2
		a <- memo(j)
		b <- memo(i-j)
	} yield (a | b) + (a.max + b.max)
	val min = tmp.map(_.size).min
	memo(i) = tmp.filter(_.size == min)
	min - 1
}

println(result.sum)
