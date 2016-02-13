import collection.BitSet

val N = 5

val full = (1 << N) - 1

def a(seq: Long, unused: BitSet): Seq[Long] =
	if (unused.isEmpty) Seq(seq >> N - 1)
	else for {
		digit <- Seq(0, 1)
		newseq = (seq << 1) | digit
		last = (newseq & full).toInt
		if unused(last)
		othr <- a(newseq, unused - last)
	} yield othr

val result = a(1, BitSet(2 to full: _*))

println(result.sum)
