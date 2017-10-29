val N = 50000000

val result = (BigInt(2) to N).iterator.map(n => 2*n*n - 1).count(_.isProbablePrime(3))
println(result)
