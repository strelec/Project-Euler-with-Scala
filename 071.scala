val bestD = (1 to 1000000).filter(_ % 7 != 0).minBy { d =>
	val n = 3 * d / 7
	3.0/7 - 1.0 * n / d
}

println(3 * bestD / 7)
