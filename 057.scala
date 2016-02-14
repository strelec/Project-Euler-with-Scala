def iter(a: BigInt, b: BigInt, add: Int = 2) = {
	val a2 = b + add*a
	val b2 = a
	val gcd = a2.gcd(b2)
	(a2/gcd, b2/gcd)
}

var a = BigInt(2)
var b = BigInt(1)

val result = (2 to 1000).count { _ =>
	val x = iter(a, b)
	a = x._1
	b = x._2
	val y = iter(a, b, 1)
	y._1.toString.size > y._2.toString.size
}

println(result)
