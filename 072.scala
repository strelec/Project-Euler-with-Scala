val N = 1000000

val sieve = helpers.Sieve(N)
val result = (2 to N).map(sieve.totients(_).toLong).sum

println(result)
