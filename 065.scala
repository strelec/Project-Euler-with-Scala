val stream = Stream(2) ++ Stream.from(1).flatMap(x => Seq(1, 2*x, 1))

val head #:: tail = stream.take(100).reverse

var a = BigInt(head)
var b = BigInt(1)

tail.foreach { i =>
	val tmpa = a
	a = b + i*a
	b = tmpa
}

println(a.toString.map(_.asDigit).sum)