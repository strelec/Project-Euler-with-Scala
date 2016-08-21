/*
	I have later realized there is a really simple closed-form formula available for this problem,
	but I am keeping the numerical solution, as it tries to do things "the unconditional way".
*/

val N = 300

val h = 100.0
val v = 20.0
val g = 9.81

def f(y: Double, fi: Double) = {
	val c = h - y
	val b = math.tan(fi)
	val temp = v * math.cos(fi)
	val a = -g / 2 / temp / temp
	
	(-b - math.sqrt(b*b - 4*a*c)) / 2 / a
}

def max(y: Double) = {
	val argmin = helpers.Helpers.minimize(0.0 to math.Pi / 2 by 10e-8, -f(y, _))
	f(y, argmin)
}

val result = {
	val step = 1.0/N

	var sum = max(0) * max(0)
	var y = step
	var stop = false

	while (!stop) {
		val x = max(y)
		if (x.isNaN) {
			stop = true;
			y -= step
			sum -= max(y) * max(y)
		} else {
			y += step
			sum += 2 * x * x
		}
	}

	math.Pi * (sum * step / 2)
}

println("%.4f" format result)
