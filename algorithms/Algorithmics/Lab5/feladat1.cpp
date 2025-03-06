/*
Nagy Csongor, 513-as csoport, 2023-24_Lab5/01

Kijelentés
Generáljunk minden lehetséges sorozatot, amelynek elemei a 0, 1, 2 halmazból
vannak, és a 0 𝑚-szer, az 1 𝑝-szer és a 2 𝑞-szor fordul elő.
*/

#include <iostream>
#include <vector>

using namespace std;

void kiir(vector<int> zarojelek, int n) {
	for (int i = 0; i < n; i++) {
		cout << zarojelek[i] << " ";
	}
	cout << endl;
	return;
}

void zarojel(int m, int p, int q, vector <int>& szamok, int k, int limit) {
	if (k == limit) {
		kiir(szamok, limit);
		return;
	}
	if (m > 0) {
		szamok[k] = 0;
		zarojel(m - 1, p, q, szamok, k + 1, limit);
	}
	if (p > 0) {
		szamok[k] = 1;
		zarojel(m, p - 1, q, szamok, k + 1, limit);
	}
	if (q > 0) {
		szamok[k] = 2;
		zarojel(m, p, q - 1, szamok, k + 1, limit);
	}
}

void beolvas(int &m, int &p, int &q, vector<int> &szamok) {
	cin >> m >> p >> q;
	for (int i = 0; i < m + p + q; i++) {
		szamok.push_back(0);
	}
}

int main() {
	int m, p, q, limit;
	vector<int> szamok;
	beolvas(m, p, q, szamok);
	limit = m + p + q;
	zarojel(m, p, q, szamok, 0, limit);

	return 0;
}