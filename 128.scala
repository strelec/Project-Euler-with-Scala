val N = 2000

def f(n: Int, p: Int) =
	3L*n*(n+1) + p

val gen = for {
	n <- Iterator.from(1)
	(t, a) <- Seq(
		f(n,1) -> Seq(f(n-2,2), /*f(n-1,1),*/ f(n-1,2), /*f(n,0),*/ f(n+1,0)/*, f(n+1,1)*/),
		f(n,2) -> Seq(/*f(n-1,2),*/ /*f(n,3),*/ f(n+1,1), /*f(n+1,2),*/ f(n+1,3), f(n+2,1))
	)
	diff = a.map(_ - t).map(math.abs).map(BigInt(_))
	if diff.count(_.isProbablePrime(5)) == 3
} yield t -> diff

println(gen.drop(N-2).next._1)
