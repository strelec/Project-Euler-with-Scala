val N = 20
val M = 50

def compute(q: Double) = {
	var x = 1
	var l = List(x/q, 1 - x/q)
	for (_ <- 2 to M) {
		x += 1
		l = (0.0 :: (l :+ 0.0)).sliding(2).map { case Seq(a, b) =>
			(1 - x/q) * a + (x/q) * b
		}.toList
	}
	0.02 - l(N)
}

val result = helpers.Helpers.findZero(50.0 to 55.0 by 1e-10, compute)
println("%.10f" format result)
