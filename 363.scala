def points(v: Double, n: Int) =
	(0 to n).iterator.map { i =>
		val t = 1.0*i/n
		val u = 1 - t
		val x = t*( 3*u*u*v + 3*t*u + t*t )
		val y = u*( u*u + 3*t*u + 3*t*t*v )
		(x, y)
	}

def length(v: Double) =
	points(v, 500000).sliding(2).map { case Seq((x1, y1), (x2, y2)) =>
		val x = x1 - x2
		val y = y1 - y2
		math.sqrt(x*x + y*y)
	}.sum

def area(v: Double) =
	points(v, 310000).sliding(2).map { case Seq((x1, y1), (x2, y2)) =>
		(x2 - x1) * (y1 + y2) / 2
	}.sum

def error(v: Double) =
	math.abs(area(v) - math.Pi/4)

val v = helpers.Helpers.minimize(0.54 to 0.56 by 1e-12, error)
val result = 100 * (length(v) - math.Pi/2) / (math.Pi/2)

println("%.10f" format result)
