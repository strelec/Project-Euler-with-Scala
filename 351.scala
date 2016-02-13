val N = BigInt(100000000)

val result = (N*N + N - 1)/2 - helpers.SumOfTotients.upTo(N)

println(6 * result)
