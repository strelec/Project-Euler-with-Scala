val N = 100000

def segmentArea(c: Double, r: Double) = {
	val phi = 2 * math.asin(c / r / 2)
	r*r * (phi - math.sin(phi)) / 2
}

def area(k: Int) = {
	def quarter(k: Int, d: Double) = {
		val x = (math.sqrt(-1 + d*d + k*(2*d + k)) - 1)/k
		val r = (k + d) / k
		x*x/2 + segmentArea(x*math.sqrt(2), r)
	}
	if (k == 1) quarter(1, 0.5)
	else quarter(k, 0.5) - quarter(k, -0.5)
}


val s = for (k <- 1 to N) yield k * area(k)
println("%.5f" format s.sum)
