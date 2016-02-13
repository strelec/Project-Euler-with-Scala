def palindrome(s: String) = s == s.reverse

val result = (0 until 1000000).filter( i =>
	palindrome(i.toString) && palindrome(BigInt(i) toString 2)
).sum

println(result)