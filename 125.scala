def isPalindromic(n: Int) = {
	var num = n;
	var rev = 0;
	while (num > 0) {
		rev = rev * 10 + num % 10;
		num = num / 10;
	}

	n == rev
}

val cumul = (0 to 5000).map {
	var s = 0L
	d => {s += d*d; s}
}

val result = (for {
	a <- cumul.indices
	b <- a+2 to cumul.size - 1
	diff = cumul(b) - cumul(a)

	if diff < 100000000
	if isPalindromic(diff.toInt)
} yield diff).distinct

println(result.sum)