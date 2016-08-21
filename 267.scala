val N = 1000
val M = 1e9

val g = { (f: Double) =>
	val l = math.log(1-f)
	(math.log(M) - N*l) / (math.log(2*f+1) - l)
}
val terms = g(helpers.Helpers.minimize(0.0 to 1 by 0.01, g)).toInt

val result = 1 - helpers.Helpers.binoms(N).map(
	_.toDouble / math.pow(2, N)
).take(terms + 1).sum

println("%.12f".format(result))
