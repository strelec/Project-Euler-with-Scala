val N = 10000000000L
val M = 4000

def rand = {
	val a = util.Random.nextLong % N
	if (a < 0) a + N else a
}

def simulate = {
	val throws = (for {
		_ <- 1 to M
		(a, b) = (rand, rand)
		s = if (a < b) a else b
		c <- Seq(s, a + b - s + 1)
	} yield c).sorted
	
	var white = true
	var pos = 0L
	
	throws.map { case p =>
		val diff = p - pos
		pos = p
		white = !white
		if (white) diff else 0
	}.sum
}

val result = (1 to 10000).map( _ =>
	N - simulate
).sum
println(result)
