val N = 20

var memo = (0 to 9).map(
	(0, _) -> 0L
).toMap.withDefaultValue(1L)

(3 to N).foreach { _ =>
	memo = (for {
		a <- 0 to 9
		b <- 0 to 9-a
		v = (0 to 9-a-b).map(
			memo(_, a)
		).sum
	} yield (a, b) -> v).toMap
}

println(memo.values.sum)
