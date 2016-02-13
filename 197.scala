def f(x: Double) = {
	val exp = 30.403243784 - x*x
	math.floor(math.pow(2, exp)) / 1e9
}

var x = -1.0
(0 to 1000).foreach { _ =>
	x = f(f(x))
}

val result = x + f(x)
println("%.9f" format result)