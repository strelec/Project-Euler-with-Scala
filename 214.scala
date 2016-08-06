val N = 40000000 - 1
val totients = sieves.Totients(N)

val lengths = Array.fill(N+1)(1)
(2 to N).foreach { i =>
	lengths(i) = 1 + lengths(totients(i))
}

val result = totients.primes.filter(lengths(_) == 25).map(_.toLong).sum
println(result)
