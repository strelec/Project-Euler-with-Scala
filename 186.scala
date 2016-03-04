val N = 1000000

val rand = Array.fill(5000000)(0)
rand.indices.foreach { kp =>
	rand(kp) = if (kp < 55) {
		val k = (kp + 1).toLong
		((100003 - 200003*k + 300007*k*k*k) % N).toInt
	} else
		(rand(kp-24) + rand(kp-55)) % N
}

var family = new helpers.DisjointSets(N)
val sizes = Array.fill(N)(1)
val minister = 524287

val result = for {
	Array(a, b) <- rand.grouped(2)
	if a != b
	as = family.find(a)
	bs = family.find(b)
} yield	if (as != bs) {
	family.union(as, bs)
	sizes(family.find(a)) = sizes(as) + sizes(bs)
	sizes(family.find(minister))
} else -1

println(1 + result.indexWhere(_ >= N*99/100))
