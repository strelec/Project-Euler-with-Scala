import helpers.Rational

val memo = {
	val digits = "123456789"
	Array.tabulate(9, 9)( (i, j) =>
		Set(Rational(("0" + digits.take(j+1).drop(i)).toInt))
	)
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

val result = memo(0)(9-1).filter(_.isNatural).map(_.toLong).sum
println(result)
