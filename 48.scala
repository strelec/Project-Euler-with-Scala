val result = (1 to 1000).map(x => BigInt(x).pow(x)).sum
println(result.toString.takeRight(10))