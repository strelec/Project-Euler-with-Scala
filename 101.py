import numpy

u = [
	sum([(-1 if j % 2 == 1 else 1) * i**j for j in range(11)])
	for i in range(1, 11)
]

def comp(i):
	vand = numpy.vander(range(1, i+1), increasing = True)
	a = numpy.linalg.solve(vand, u[:i])
	return sum([a[j] * (i+1)**j for j in range(i)])

result = sum([comp(i+1) for i in range(10)])
print("{:.0f}".format(result))
