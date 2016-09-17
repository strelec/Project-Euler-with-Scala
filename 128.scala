val N = 2000

val gen = for {
	n <- Iterator.from(1)
	if BigInt(6*n + 5).isProbablePrime(5)
	s = 3L * n * (n+1)
	(t, a) <- Seq(
		(s + 1) -> Seq(6*n - 1, 12*n - 7),
		(s + 2) -> Seq(6*n + 7, 12*n + 17)
	)
	if a.forall(BigInt(_).isProbablePrime(5))
} yield t

println(gen.drop(N-2).next)
