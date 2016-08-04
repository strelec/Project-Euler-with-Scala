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
	var a = 0.38
	var b = 1.5
	var c = b - (b-a) / gr
	var d = a + (b-a) / gr
	
	while (math.abs(c - d) > 0.000001) {
		if (f(y, c) > f(y, d)) b = d else a = c
		c = b - (b-a) / gr
		d = a + (b-a) / gr
	}
	
	f(y, (b + a) / 2)
}

println((120.0 to 120.4 by 0.001).map(gss))
