val N = 1000000000

val result = for {
	a <- 2L to N/3+1
	p <- Seq(-1, 1)
	c = a + p
	if c <= N && c > 0
	
	value = 3*a*a - 2*a*p - 1
	root = math.sqrt(value).round
	if root * root == value
	if root * c % 2 == 0 
} yield {
	a + a + c
}

println(result.sum)
