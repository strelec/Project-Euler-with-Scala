import helpers._

val binoms = (for {
	line <- 0 until 51
	binom <- Helpers.binoms(line)
} yield binom).distinct

val result = for {
	binom <- binoms
	if NativeFactoring(binom.toLong).factor.forall(_._2 == 1)
} yield binom

println(result.sum)
