import helpers.Rational

val memo = {
	val digits = "123456789"
	Array.tabulate(9, 9) { (i, j) =>
		if (i <= j)
			Set(Rational(digits.substring(i, j+1).toInt))
		else Set(Rational(0))
	}
}

for {
	i <- 1 to 8
	j <- 0 to 8-i
} memo(j)(j+i) ++= (for {
	k <- 0 until i
	a <- memo(j)(j+k)
	b <- memo(j+k+1)(j+i)
	c <- Seq(a+b, a-b, a*b, a/b)
} yield c)

val result = memo(0)(8).filter(_.isNatural).map(_.toLong).sum
println(result)
