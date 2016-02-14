type Num = Double

val ops = Seq(
	(a: Num, b: Num) => a + b,
	(a: Num, b: Num) => a - b,
	(a: Num, b: Num) => a * b,
	(a: Num, b: Num) => a / b
)

val result = (0 to 9).combinations(4).maxBy { comb =>
	val results = for {
		Seq(a, b, c, d) <- comb.permutations.toStream

		o1 <- ops
		o2 <- ops
		o3 <- ops

		result <- Seq(
			o3(o2(o1(a,b),c),d),
			o3(o1(a,b),o2(c,d)),
			o3(o2(a,o1(b,c)),d),
			o3(a,o2(o1(b,c),d)),
			o3(a,o2(b,o1(c,d)))
		)
	} yield result

	(Stream from 1).takeWhile(results.indexOf(_) != -1).size
}

println(result.mkString)