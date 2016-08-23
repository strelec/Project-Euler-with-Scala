def curve(x: Double) = {
	var p2 = 1.0
	var sum = 0.0
	for (_ <- 1 to 60) {
		var u = p2 * x % 1
		if (u > 0.5) u = 1 - u
		sum += u / p2
		p2 *= 2
	}
	sum
}

def circle(x: Double) =
	0.5 - math.sqrt(x/2 - x*x)

def integrate(x: Double, steps: Int): Double =
	if (steps <= 0) 0.0
	else if (0 <= x && x <= 0.5)
		integrate(2*x, steps-1)/4 + x*x/2
	else if (0.5 <= x && x <= 1)
		0.5 - integrate(1-x, steps-1)
	else {
		val n = math.floor(x)
		n/2 + integrate(x - n, steps-1)
	}

def areaUnderCircle(h: Double) = {
	val R = 0.25
	val r = R - h
	// S is the area of the circular segment
	val S = R*R * math.acos(r/R) - r * math.sqrt(h*(R + r))
	(0.5 - h - R*R*math.Pi + S)/2
}

val p = helpers.Helpers.findZero(0.0 to 0.4 by 1e-10, {
	(x: Double) => curve(x) - circle(x)
})
val result = 0.25 - integrate(p, 20) - areaUnderCircle(p)

println("%.8f" format result)
