val N = 10000000 - 1
val totients = helpers.Sieve(N).totients

val result = totients.indices.drop(2).filter( i =>
	i.toString.sorted == totient(i).toString.sorted 
).minBy( i =>
	1.0 * i / totients(i)
)

println(result)
