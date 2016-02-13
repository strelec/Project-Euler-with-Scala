val str = (1 to 200000).flatMap(_.toString)

val result = for {
	p <- 0 to 6
	n = BigInt(10).pow(p).toInt - 1
} yield str(n).asDigit

println(result.product)