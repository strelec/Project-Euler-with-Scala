#include <iostream>
#include <algorithm>
#include <vector>
#include <cmath>
using namespace std;

typedef unsigned int uint;

uint N = 10000;
double PI = acos(-1);

double f(uint k) {
	return exp(1.0 * k / N) - 1;
}

vector<pair<double, uint>> computePairs() {
	uint maxn = N;
	while (f(maxn) < PI) maxn++;
	maxn--;

	vector<double> singles(maxn);
	for (uint i = 0; i < maxn; i++)
		singles[i] = f(i+1);
		
	vector<pair<double, uint>> pairs;
	pairs.reserve(N * N * 3/4);
	for (uint i = 0; i < maxn; i++) {
		for (uint j = i; j < maxn; j++) {
			double a = singles[i];
			double b = singles[j];
			if (a + b < PI)
				pairs.push_back({a + b, (i+1)*(i+1) + (j+1)*(j+1)});
		}
	}
	sort(pairs.begin(), pairs.end());
	return pairs;
}

int main(int argc, char* argv[]) {
	double best = 100;
	uint result = 0;
	
	auto pairs = computePairs();
	uint i = 0;
	uint j = pairs.size() - 1;
	while (i <= j) {
		double cur = pairs[i].first + pairs[j].first;
		double diff = abs(PI - cur);
		if (diff < best) {
			best = diff;
			result = pairs[i].second + pairs[j].second;
		}
		if (cur > PI) j--; else i++;
	}
	
	cout << result << endl;
}
