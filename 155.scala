import helpers.Rational

val N = 18

val result = Array.fill(N+1)(Seq.empty[Rational])
result(1) = Seq(Rational(60))

(2 to N).foreach { n =>
	result(n) = (for {
		i <- (1 to (n+1)/2)
		a <- result(i)
		b <- result(n-i)
		c <- Seq(a+b, (a.inv+b.inv).inv)
	} yield c).distinct
}

println(result.flatten.distinct.size)
