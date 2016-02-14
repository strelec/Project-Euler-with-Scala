import helpers._

val sieve = Sieve(100000)

var k = 0
var count = 0

while (k < 10 || 10*count >= 4*k + 5) {
	count += (0 to 3).count( m =>
		sieve.isPrime(4*k*k + 12*k + 9 - 2*(k+1)*m)
	)
	k += 1
}

println((k+1)*2 + 1)