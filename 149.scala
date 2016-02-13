val N = 2000
val s = Array.fill(N*N)(0)

s.indices.foreach( kp =>
	s(kp) = if (kp < 55) {
		val k = (kp + 1).toLong
		((100003 - 200003*k + 300007*k*k*k) % 1000000 - 500000).toInt
	} else
		(s(kp-24) + s(kp-55) + 1000000) % 1000000 - 500000
)

val result = for {
	S <- 0 until N
	direction <- Seq(
		S until N*N by N, // vertical
		S until (N-S)*N by N+1, // diagonal
		(S+1)*N until N*N by N+1, // diagonal
		S*N until (S+1)*N, // horizontal
		S until S*N+1 by N-1, // antidiagonal
		(S+2)*N-1 until N*N by N-1 // antidiagonal
	)
} yield	{
	var cur = 0
	var max = 0
	direction.foreach { el =>
		cur += s(el)
		if (cur < 0)
			cur = 0
		if (max < cur)
			max = cur
	}
	max
}

println(result.max)
