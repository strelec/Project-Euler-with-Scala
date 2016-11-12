/*
	EXPLANATION:
	
	A1    B4 C1
	A2 A7 A8 B3
	A3    B2
	A4 A5 A6 B1
	
	1. Writing down the system of equations, the problem has 8 degrees of freedom, representing the "A" variables on above diagram.
	2. Try 0 .. 9 without constraints for each of A1 - A4. Their sum is the magic constant S.
	3. For A5 - A8 try 0 .. 9, but apply constraints and compute "B" variables alongside, which can be done trivially.
	4. We are left with 4 fields, none trivialy computable. We derive C1 = (S - A8 - A4 + A7 + A5 - A1 - B4)/2. Check if C1 in 0 .. 9.
	5. Having computed C1, the other three are again trivial. Just make sure they fall on 0 .. 9.
*/

val r = 0 to 9
var result = 0
for {
	a1 <- r; a2 <- r; a3 <- r; a4 <- r
	s = a1 + a2 + a3 + a4

	a5 <- (0 max s-a4-2*9) to (9 min s-a4)
	a6 <- (0 max s-a4-a5-9) to (9 min s-a4-a5)
	b1 = s - a4 - a5 - a6
	
	a7 <- (0 max s-a1-b1-9) to (9 min s-a1-b1)
	b2 = s - a1 - a7 - b1
	
	a8 <- (0 max s-a2-a7-9 max s-a6-b2-9) to (9 min s-a2-a7 min s-a6-b2)
	b3 = s - a2 - a7 - a8
	b4 = s - a6 - b2 - a8
	
	c = s - a8 - a4 + a7 + a5 - a1 - b4
	if c % 2 == 0
	c1 = c / 2
	if r.contains(c1)
	if r.contains(s - b1 - b3 - c1)
	if r.contains(s - a1 - b4 - c1)
	if r.contains(s - a4 - a8 - c1)
} result += 1

println(result)
