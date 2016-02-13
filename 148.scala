val N = 1000000000

var sum = 0L
(0 until N).foreach { num =>
	var n = num
	var prod = 1
	while(n != 0) {
		prod *= n % 7 + 1
		n /= 7
	}
	sum += prod
}

println(sum)
