val N = 1000
val LIMIT = 20000

def cubes(a: Int, b: Int, c: Int, n: Int) =
	2*(a*b + b*c + a*c) + 4*(n-1)*(a + b + c) + 4*(n-1)*(n-2)

val counts = Array.fill(LIMIT+1)(0)

for {
	a <- Iterator.from(1).takeWhile(a => cubes(a,a,a,1) <= LIMIT)
	b <- Iterator.from(a).takeWhile(b => cubes(a,b,b,1) <= LIMIT)
	c <- Iterator.from(b).takeWhile(c => cubes(a,b,c,1) <= LIMIT)
	n <- Iterator.from(1).takeWhile(n => cubes(a,b,c,n) <= LIMIT)
} counts(cubes(a, b, c, n)) += 1

counts.indexOf(N) match {
	case -1 => println("Please increase LIMIT.")
	case  i => println(i)
}
