/*
Kijelentés
Ismert sejtés, miszerint ha egy adott 𝑛 természetes számot ismételten alávetünk
a következő feldolgozásnak, eljutunk az 1-es számhoz: ha 𝑛 páros, akkor elosztjuk
2-vel, ha 𝑛 páratlan, megszorozzuk 3-mal és az eredményhez hozzáadunk 1-et.
Azoknak a lépéseknek a számát, amelyek egy adott 𝑛-ből az 1-hez vezetnek,
karakterisztikának nevezzük. Írjunk programot, amely meghatározza azt az 𝑛
számot, amely az [𝑥, 𝑦] intervallumhoz tartozik és amelynek a karakterisztikája
a legnagyobb!
*/

#include <iostream>

using namespace std;

int countCharacteristics(int x) {
	int count = 0;
	while (x != 1) {
		if (x % 2 == 1) {
			x = 3 * x + 1;
		}
		else x = x / 2;
		count++;
	}
	return count;
}

int searchForMax(int* a, int n, int& indexOfMax) {
	int max = a[0];
	for (int i = 1; i < n; i++) {
		if (max < a[i]) {
			max = a[i];
			indexOfMax = i;
		}
		if (max == a[i]) {
			indexOfMax = i;
		}
	}
	return max;
}

int main() {
	int x, y, db = 0, max;
	cin >> x >> y;
	int* a = new int[y - x];

	for (int i = x; i <= y; i++) {
		a[db] = countCharacteristics(i);
		db++;
	}
	int indexOfMax = 0;
	max = searchForMax(a, db, indexOfMax);

	cout << x + indexOfMax <<" "<< max << endl;

	return 0;
}