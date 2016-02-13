val max = 10000
val out = (1 to max).iterator.map(x => x*(x+1)/2).find { n =>
	println(n)
	println( (1 to n).count(n % _ == 0) )
	500 < (1 to n).count(n % _ == 0)
}

println(out)