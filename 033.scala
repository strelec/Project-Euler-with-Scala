val result = for {
	n <- 10 to 99
	if n % 10 != 0
	d <- n+1 to 99
	ns = n.toString
	ds = d.toString
	
	common = ns intersect ds
	if common.exists( c =>
		n * (ds diff Seq(c)).toInt == d * (ns diff Seq(c)).toInt
	)
} yield (n, d)

val n = result.map(_._1).product
val d = result.map(_._2).product

println(d / helpers.Helpers.gcd(n, d))
