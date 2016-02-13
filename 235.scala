def s(r: Double) = (1 to 5000).map( k =>
	(900 - 3*k) * math.pow(r, k-1)
).sum

var lo = 1.002
var hi = 1.003

(0 to 50).foreach { _ =>
	val mid = (hi + lo)/2
	if (s(mid) < -6e11) {
		hi = mid
	} else {
		lo = mid
	}
}

val result = lo + 5e-13

println(result.toString.take(2 + 12))
