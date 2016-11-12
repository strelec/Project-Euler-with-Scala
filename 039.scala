val result = (0 to 1000).maxBy( p =>
	(for {
		c <- 1 to p
		b <- 1 to (c min p-c)
		a = p - b - c
	} yield a*a + b*b == c*c).count(identity)
)

println(result)
