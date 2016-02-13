/*
	EXPLANATION:
	
	1. We create an array with 1,500,000 zeros.
	2. With the Euclid m, n parametrization, the circumference of the triangle is c = 2 * m * (m + n)
	3. Therefore we try all the m and n until we hit c <= 1,500,000
	4. if m and n are coprime, we add 1 to each multiple of c in the count array
	5. We count the number of 1s in the array
*/

val N = 1500000

val counts = Array.fill(N+1)(0)

for {
	n <- Iterator.from(1).takeWhile( n => 4*n*n <= N )
	m <- Iterator.from(n+1, 2).takeWhile( m => 2*m*(m+n) <= N )
	if helpers.Helpers.gcd(m, n) == 1
	o = 2*m*(m+n)
	k <- 1 to N/o
} counts(k*o) += 1

println(counts.count(_ == 1))
