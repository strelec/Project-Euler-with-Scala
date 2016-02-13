var bouncy = 0

var n = 90
while(99*n > 100*bouncy) {
	n += 1

	val s = n.toString
	val ss = s.sorted
	if (s != ss && s != ss.reverse)
		bouncy += 1
}

println(n)
