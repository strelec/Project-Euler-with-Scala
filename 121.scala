val N = 15
val probs = Seq.tabulate(N)(_ + 2).toSet

val result = for {
	c <- N/2 + 1 to N
	ss <- probs.subsets(c)
	in = ss.toSeq.map(1.0 / _)
	out = (probs &~ ss).toSeq.map( i => 1.0*(i-1)/i )
} yield in.product * out.product

println((1 / result.sum).toInt)
