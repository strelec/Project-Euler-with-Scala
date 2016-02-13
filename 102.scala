val file = io.Source.fromURL("https://projecteuler.net/project/resources/p102_triangles.txt").getLines

def parse(s: String) =
	s.split(",").map(_.toInt).grouped(2).map {
		case Array(a, b) => (a, b)
	}.toList
	
def side(a: (Int, Int), b: (Int, Int)) =
	(a._1 - b._1) * b._2 - b._1 * (a._2 - b._2) <= 0

val result = file.map(parse).count { case Seq(a, b, c) =>
	val d = side(a, b)
	val e = side(b, c)
	d == e && e == side(c, a)
}
println(result)

