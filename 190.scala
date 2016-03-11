val N = 15

def p(m: Int) = if (m <= 2) 1 else {
	val d = m-2
	val s = (d to m*d by d).sum/m
	(1 to m).map( i =>
		math.pow(1.0*d*i/s, i)
	).product.toInt
}

println((2 to N).map(p).sum)
