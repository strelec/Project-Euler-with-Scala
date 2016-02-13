/*
	EXPLANATION:
	We produce an iterator that produces results in order. Therefore we just take the first element at the end and minimize computation via the smart pruning.
	
	1. Since the sequence starts with the smallest number, we fix it first. We try with 6 and count downwards to 1 in the worst case. Let we call it S.
	2. Now, since the sequence is 16 digits long, 10 is on the outside. Therefore S and 10 are on the outside. We need to pick 3 numbers more, but they have to above S.
	3. We fix S on the outside and try all 4! = 24 permutations of 10 and previous 3-set. We now have the whole sequence outside of the pentagon.
	
	4. Now we try all the possible pairs of numbers in line with S to determine the magic number. We try them in order to not spoil the iterator property.
	5. From now on, no more branching. All other numbers are trivially computable, we just have to check that the circle completes nicely and that all 10 numbers are used.
*/

val result = for {
	smallest <- (6 to 1 by -1).iterator
	choose3  <- (smallest+1 to 9).toSet.subsets(3)
	permuted <- (10 :: choose3.toList).permutations
	outside   = smallest :: permuted
	
	remaining = (10 to 1 by -1).diff(outside)
	inside1  <- remaining
	inside2  <- remaining
	if inside1 != inside2
	
	magic  = smallest + inside1 + inside2
	inside = outside.scanLeft(inside1)(magic - _ - _)
	if inside.last == inside.head
	if remaining.diff(inside).isEmpty
	
} yield (outside, inside.sliding(2).toSeq).zipped.map {
	case (a, List(b, c)) => s"$a$b$c"
}.mkString

println(result.next)
