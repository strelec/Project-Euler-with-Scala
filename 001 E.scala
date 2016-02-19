/*
	EXPLANATION:
	
	1. Instead of looping through all numbers 1 .. N, we have a constant time solution here (for fixed bit integers).
	2. You can compute sum of multiples in constant time, the formula is built in into scala (sum on Range object)
	3. Since we have added the multiples of 15 twice, we have to subtract them once.
*/

val N = 1000 - 1
def sum(i: Int) = (i to N by i).sum 
println(sum(3) + sum(5) - sum(15))
