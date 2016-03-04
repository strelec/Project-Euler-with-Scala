import collection.immutable.BitSet

val table = {
	def opts(n: Int): Vector[BitSet] =
		if (n == 0) Vector(BitSet())
		else if (n < 0) Vector()
		else for {
			brick <- Vector(2, 3)
			prev <- opts(n - brick)
		} yield prev + n

	opts(32).map(_ - 32)
}

var memo = Array.fill(10)(Seq.empty[Long])
memo(0) = table.map(_ => 1L)

memo.indices.tail.foreach { t =>
	memo(t) = table.map( row =>
		(for {
			(v, i) <- memo(t-1).zipWithIndex
			if (row & table(i)).isEmpty
		} yield v).sum
	)
}

println(memo.last.sum)
