val file = io.Source.fromURL("https://projecteuler.net/project/resources/p067_triangle.txt").getLines

val triangle = file.map(_.split(" +").map(_.toInt).toArray).toArray

def get(r: Int, c: Int) =
	if (r < 0 || c < 0 || c >= triangle(r).size)
		0
	else triangle(r)(c)

for {
	r <- triangle.indices
	c <- triangle(r).indices
} triangle(r)(c) += get(r-1, c-1) max get(r-1, c)

println(triangle.last.max)
