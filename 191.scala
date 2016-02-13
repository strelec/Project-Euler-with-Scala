var a = Vector(1,2,4)

for (i <- 0 until 28) {
	a :+= a.takeRight(3).sum
}

val result = a(30) + (0 to 29).map(i => a(i)*a(29-i)).sum

println(result)