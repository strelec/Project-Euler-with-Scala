val N = 10000000 - 1
val totients = sieves.Totients(N)

val result = (2 to N).filter( i =>
	i.toString.sorted == totients(i).toString.sorted 
).minBy( i =>
	1.0 * i / totients(i)
)

println(result)
