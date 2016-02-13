
def f(num: Long) = {
	println(num)
	var n: BigInt = 1
	while(n % num != 0) {
		n += 1
		var d: BigInt = 1
		while (n / d % 10 > 2) {
			d *= 10
			n -= n % d
			n += d
		}
	}
	n / num
}

//println(f(9999))

val result = (1L to 10000L).map(f).sum

//println(result)

