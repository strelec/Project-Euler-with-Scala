case class Point(x: Double, y: Double) {
	def reflect(p: Point) = {
		val a = p.y / p.x / 4
		val c = p.y * 3   / 4
		
		val d = (x + a*(y - c)) / (1 + a*a)
		Point(2*d - x, 2*d*a - y + 2*c)
	}
	
	def pinpoint(p: Point) = {
		val a = (p.y - y) / (p.x - x)
		val c = y - a*x
		
		val x2 = Seq(1, -1).map( s =>
			(2*s*math.sqrt(25*a*a - c*c + 100) - a*c) / (4 + a*a)
		).maxBy( x2 => math.abs(x2 - x) )

		Point(x2, a*x2 + c)
	}
}

var p1 = Point(0, 10.1)
var p2 = Point(1.4, -9.6)
var result = 0

while (p2.x > 0.01 || p2.x < -0.01 || p2.y < 0) {
	val p3 = p1.reflect(p2)
	val p4 = p2.pinpoint(p3)
	p1 = p2
	p2 = p4
	result += 1
}

println(result)
