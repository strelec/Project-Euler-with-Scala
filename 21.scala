import helpers._

val result = (2 until 10000).filter { i =>
	val other = NativeFactoring(i).divisorSum - i
	i != other && i == NativeFactoring(other).divisorSum - other
}

println(result.sum)
