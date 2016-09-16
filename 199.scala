import math.{sqrt, Pi}

val N = 10

def innerRadius(a: Double, b: Double, c: Double) =
	a*b*c / (a*b + b*c + a*c + 2*sqrt(a*b*c*(a+b+c)))

def outerRadius(a: Double, b: Double) =
	a*b / (a + b - a*b + 2*sqrt(a*b*(1-a-b)))

def inner(a: Double, b: Double, c: Double, i: Int): Double = {
	val r = innerRadius(a, b, c)
	val p = Pi * r * r
	i match {
		case 0 => p
		case _ =>
			p + inner(a, b, r, i-1) + inner(a, c, r, i-1) + inner(b, c, r, i-1)
	}
}

def outer(a: Double, b: Double, i: Int): Double = {
	val r = outerRadius(a, b)
	val p = Pi * r * r
	i match {
		case 0 => p
		case _ =>
			p + inner(a, b, r, i-1) + outer(a, r, i-1) + outer(b, r, i-1)
	}
}

val r = 2*sqrt(3) - 3
val rec = inner(r,r,r,N-1) + 3*outer(r,r,N-1)
val result = 1 - (rec / Pi + 3*r*r)
	
println("%.8f" format result)
