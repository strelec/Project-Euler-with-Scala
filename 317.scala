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

val gr = (math.sqrt(5) + 1) / 2

def gss(y: Double) = {
	var a = 0.0
	var b = math.Pi / 2
	var c = b - (b-a) / gr
	var d = a + (b-a) / gr
	
	while (math.abs(c - d) > 10e-8) {
		if (f(y, c) > f(y, d)) b = d else a = c
		c = b - (b-a) / gr
		d = a + (b-a) / gr
	}
	
	f(y, (b + a) / 2)
}

val result = {
	val step = 1.0/N

	var sum = gss(0) * gss(0)
	var y = step
	var stop = false

	while (!stop) {
		val x = gss(y)
		if (x.isNaN) {
			stop = true;
			y -= step
			sum -= gss(y) * gss(y)
		} else {
			y += step
			sum += 2 * x * x
		}
	}

	math.Pi * (sum * step / 2)
}

println("%.4f" format result)
