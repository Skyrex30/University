/*
Nagy Csongor, 513-as csoport, 2023-24_Lab6/01

Kijelentés
Adott egy 𝑛 × 𝑚-es mátrix. Határozzuk meg azt a legnagyobb 𝑛 elemű
összeget, amelyet úgy kapunk meg, hogy minden elem a mátrix különböző
soraiból “származik” és az 𝑖. sorból kiválasztott elem kisebb mint az 𝑖 + 1. sorból
kiválasztott elem.
*/

#include <iostream>
#include <vector>
#include <climits>

using namespace std;

void beolvas(int &n, int &m, vector<vector<long long>>& szamok) {
	cin >> n >> m;

	szamok.resize(n);
	
	for (int i = 0; i < n; i++) {
		szamok[i].resize(m);
		for (int j = 0; j < m; j++) {
			cin >> szamok[i][j];
		}
	}
	return;
}

void megold(int n, int m, vector<vector<long long>> szamok) {

	long long sum = 0, max = 0;
	bool ok = true;
	long long maxOld = szamok[n - 1][0];

	for (int i = 1; i < m; i++) {
		if (szamok[n - 1][i] > maxOld) {
			maxOld = szamok[n - 1][i];
		}
	}

	sum += maxOld;

	int i = n - 2;
	while (i >= 0 && ok) {
		max = LLONG_MIN;

		for (int j = 0; j < m; j++) {
			if (szamok[i][j] > max && szamok[i][j] < maxOld) {
				max = szamok[i][j];
			}

		}

		if (max >= maxOld || max == LLONG_MIN) {
			ok = false;
		}
		sum += max;
		maxOld = max;
		i--;
	}
	
	if (ok) cout << sum;

	else cout << "nincs megoldas";

	return;
}

int main() {
	int n = 0, m = 0;
	vector<vector<long long>> szamok;

	beolvas(n, m, szamok);
	megold(n, m, szamok);

	return 0;
}