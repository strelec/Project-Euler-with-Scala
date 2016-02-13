val result = for {
	power <- 1 to 21
	digit <- 1 to 9
	number = Seq.fill(power)(BigInt(digit)).product
	if power == number.toString.size
} yield number

println(result.size)