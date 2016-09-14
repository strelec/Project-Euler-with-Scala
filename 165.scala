import helpers.Rational

case class Point(x: Int, y: Int)
case class Line(a: Point, b: Point)

val lines = {
	val generator = new Iterator[Int] {
		var cur = 290797L
	
		def next = {
			val result = cur % 500
			cur *= cur
			cur %= 50515093
			result.toInt
		}
	
		def hasNext = true
	}
	generator.next

	generator.grouped(4).map { case Seq(x1, y1, x2, y2) =>
		Line(Point(x1, y1), Point(x2, y2))
	}.take(5000).toArray
}

def sort[T <% Ordered[T]](t: (T, T)) =
	if (t._1 <= t._2) t else t.swap

def range(e: (Int, Int), f: (Int, Int)) = {
	val ((a, b), (c, d)) = sort((sort(e), sort(f)))
	if (b > d) Some((c, d))
	else if (b > c) Some((c, b))
	else None
}

val result = for {
	i <- lines.indices.iterator
	a = lines(i)
	j <- lines.indices.drop(i+1)
	b = lines(j)
	
	(r1x, r2x) <- range((a.a.x, a.b.x), (b.a.x, b.b.x))
	(r1y, r2y) <- range((a.a.y, a.b.y), (b.a.y, b.b.y))
	
	dx1 = a.a.x - a.b.x
	dx2 = b.a.x - b.b.x
	dy1 = a.a.y - a.b.y
	dy2 = b.a.y - b.b.y
	
	d = dx1*dy2 - dy1*dx2
	if d != 0
	
	c1 = a.a.x*a.b.y - a.a.y*a.b.x
	c2 = b.a.x*b.b.y - b.a.y*b.b.x
	
	x = Rational(dx2*c1 - dx1*c2, d).simplify
	if x >= Rational(r1x) && x <= Rational(r2x)
	
	y = Rational(dy2*c1 - dy1*c2, d).simplify
	if y >= Rational(r1y) && y <= Rational(r2y)
	
	if x != Rational(a.a.x) || y != Rational(a.a.y)
	if x != Rational(b.a.x) || y != Rational(b.a.y)
	if x != Rational(a.b.x) || y != Rational(a.b.y)
	if x != Rational(b.b.x) || y != Rational(b.b.y)
} yield (x, y)

println(result.toSet.size)

