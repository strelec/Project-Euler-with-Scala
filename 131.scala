val N = 1000000 - 1

val sieve = helpers.Sieve(N)
val it = Iterator.from(2).map( x => 3L*x*x - 3*x + 1 ).takeWhile(_ < N)

println(it.count(sieve.isPrime))
