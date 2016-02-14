def fifthPower(x: Int) = x*x*x*x*x

val result = (10 to 354294).filter( i =>
	i == i.toString.map(_.asDigit).map(fifthPower).sum
)

println(result.sum)
