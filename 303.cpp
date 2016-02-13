#include <iostream>
using namespace std;

typedef unsigned long long ulong;

ulong f(ulong num) {
	ulong n = 1;
	while(n % num != 0) {
		n += 1;
		ulong d = 1;
		while (n / d % 10 > 2) {
			d *= 10;
			n -= n % d;
			n += d;
		}
	}
	return n;
}

int main(int argc, char* argv[]) {
	long sum = 0;
	for (int i = 1; i <= 10000; ++i) {
		ulong result = f(i);
		sum += result / i;
		cout << i << " " << result << endl;
	}
	cout << "SUM: " << sum << endl;
}
