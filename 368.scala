def consec(num: Long): Boolean = {
	var n = num
	var rep = 0
	var d = -1L
	while (n != 0) {
		val digit = n % 10
		if (digit == d) {
			rep += 1
			if (rep > 2)
				return true
		} else {
			d = digit
			rep = 1
		}
		n /= 10
	}
	return false
}

var sum = 0.0
var i = 1L
while(true) {
	var j = 0
	while(j < 100000000) {
		if (!consec(i))
			sum += 1.0/i
		i += 1
		j += 1
	}
	println(sum)
}
