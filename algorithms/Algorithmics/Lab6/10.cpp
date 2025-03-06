#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

void input(int& n, vector<pair<uint64_t, int>>& zsakok) {
	fin >> n;

	for (int i = 0; i < n; i++) {
		uint64_t num;
		fin >> num;
		zsakok.push_back(make_pair(num, i));		
	}
}

void out(vector<pair<pair<int, int>, uint64_t>>& megoldas, uint64_t db) {
	cout << db << endl;
	for (uint64_t i = 0; i < megoldas.size(); i++) {
		cout << megoldas[i].first.first << " " << megoldas[i].first.second << " " << megoldas[i].second << endl;
	}
}

uint64_t calculateAvg(int n, vector<pair<uint64_t, int>> zsakok) {
	uint64_t sum = 0;
	for (int i = 0; i < n; i++) {
		sum += zsakok[i].first;
	}
	sum /= n;

	return sum;
}

uint64_t solve(int n, uint64_t avg, vector<pair<uint64_t, int>> zsakok, vector<pair<pair<int, int>, uint64_t>> &megoldas) {
	uint64_t db = 0;
	for (int i = 0; i < n; i++) {
		for (int j = 0; j < n; j++) {
			if (zsakok[i].first != 0 && zsakok[i].first != avg && avg - zsakok[j].first == zsakok[i].first - avg) {		// a pairben az elso a zsakban levo ermek szama, a masodik az eredeti indexe		
				megoldas.push_back(make_pair(make_pair(zsakok[i].second + 1, zsakok[j].second + 1), avg - zsakok[j].first));
				db++;
				zsakok[i].first = avg;
				zsakok[j].first = avg;
			}
			uint64_t kulonbseg = zsakok[i].first - avg;
			if (kulonbseg != 0 && (kulonbseg + zsakok[j].first) < avg && zsakok[i].first && zsakok[i].first - kulonbseg == avg) {
				megoldas.push_back(make_pair(make_pair(zsakok[i].second + 1, zsakok[j].second + 1), kulonbseg));
				db++;
				zsakok[i].first = avg;
				zsakok[j].first += kulonbseg;
			}
		}
	}	

	return db;
}

bool compare(pair<uint64_t, int>& a, pair<uint64_t, int>& b) {
	return a.first > b.first;
}

int main() {
	int n;
	vector<pair<uint64_t, int>> zsakok;

	input(n, zsakok);

	sort(zsakok.begin(), zsakok.end(), compare);

	uint64_t avg = calculateAvg(n, zsakok);

	vector<pair<pair<int, int>, uint64_t>> megoldas;

	uint64_t db = solve(n, avg, zsakok, megoldas);

	out(megoldas, db);

	return 0;
}