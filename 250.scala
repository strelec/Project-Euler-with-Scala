val N = 250
val M = 250250
val MOD = BigInt(10).pow(16).toLong

val counts = Array.fill(N)(0)
for (i <- 1 to M)
	counts(BigInt(i).modPow(i, N).toInt) += 1

val memo = Array.fill(N)(0L)
memo(0) = BigInt(2).modPow(counts(0), MOD).toLong

for {
	i <- counts.indices.tail
	_ <- 1 to counts(i)
} {
	val old = memo.clone
	for (j <- old.indices) {
		memo(j) += old((j + i) % N)
		memo(j) %= MOD
	}
}

println(memo(0) - 1)
