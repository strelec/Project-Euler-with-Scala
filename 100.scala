val r2 = math.sqrt(2)

def red(n: Int) =
	(math.pow(3+2*r2, n) - math.pow(3-2*r2, n))/4/r2

def blue(red: Double) =
	(math.sqrt(8*red*red + 1) + 2*red + 1)/2
	
val result = (15 to 18).map { n =>
	val r = red(n)
	blue(r) -> r
}.find( x =>
	x._1 + x._2 > 1000000000000.0
)

println(result.get._1.round)
