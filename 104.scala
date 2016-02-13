def fib(a: BigInt = 0, b: BigInt = 1): Stream[BigInt] =
	a #:: fib(b, a+b)

def pandigital(a: BigInt) = {
	val map = a.toString.groupBy(identity)
	map.size == 9 && !map.isDefinedAt('0')
}

def firstNine(a: BigInt) = {
	val trim = ((a.bitLength - 37) * math.log10(2)).toInt
	var result = a / BigInt(10).pow(trim)
	while (result >= 1000000000)
		result /= 10
	result
}

def lastNine(a: BigInt) =
	a % 1000000000


var a = BigInt(0)
var b = BigInt(1)
var i = 0

while(i < 2749 || !pandigital(lastNine(a)) || !pandigital(firstNine(a))) {
	println(i)

	i += 1
	val tmp = b
	b = a + b
	a = tmp
}

println(i)