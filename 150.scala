val N = 1000

val randoms = {
	var t = 0L
	Array.fill(N*(N+1)/2) {
		t = ((615949*t + 797807) % (1 << 20) + (1 << 20)) % (1 << 20)
		t - (1 << 19)
	}.reverse
}
var k = 0

var a2 = Array.fill(N+2, N+1)(0L)
var a1 = Array.fill(N+2, N+1)(0L)
var a0 = Array.fill(N+2, N)(0L)

var result = Long.MaxValue

(0 until N).foreach { step =>
	val cols = N - step
	
	for {
		 i <- 0+2 to step+2
		 j <- 0 until cols
	} {
		a0(i)(j) = randoms(k+j) + a1(i-1)(j) + a1(i-1)(j+1) - a2(i-2)(j+1)
		result = result min a0(i)(j)
	}
	k += cols
	
	val tmp = a2
	a2 = a1
	a1 = a0
	a0 = tmp
}

println(result)
