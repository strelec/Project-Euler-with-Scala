def area(k: Double) = {
	import math.sqrt
	val point = 1 / (1 + k + sqrt(2*k))
	def integralA(x: Double) = k*x*x/2
	def integralB(x: Double) = {
		val sq = sqrt(1 - x*x)
		x - x*sq/2 + math.atan(x * sq / (x*x - 1))/2
	}
	integralA(point) - integralA(0) + integralB(1 - point) - integralB(0)
}

val total = 1 - math.Pi/4
val result = Iterator.from(1).find( k =>
	area(1.0/k) / total < 0.001
).get
println(result)
