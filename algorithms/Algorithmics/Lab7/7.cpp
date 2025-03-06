/* Nagy Csongor, 513-s csoport
* Lab 7/07
Kijelentés
Adott 𝑛 elemű 𝑎 sorozat, melyre igaz, hogy 𝑎1 > 𝑎2 és 𝑎𝑛−1 < 𝑎𝑛. Lokális
minimumnak nevezünk minden olyan 𝑎𝑖 elemet, amelyre 𝑎𝑖−1 > 𝑎𝑖 < 𝑎𝑖+1 és
𝑖 = 2, 𝑛 − 1. Határozzunk meg egy tetszőleges lokális minimumot egy tömbben,
melynek kezdetben ismeretlenek az elemei!
*/


#include <iostream>
#include <vector>
#include <climits>
using namespace std;

int megold(vector<uint64_t> &a, int b, int j) {
	int k = (b + j) / 2;

	if (b > j) return -1; 

	if (a[k] == 0) {
		cout << 0 << " " << k << endl;
		cin >> a[k];
	}
	if (a[k + 1] == 0) {
		cout << 0 << " " << k + 1 << endl;
		cin >> a[k + 1];
	}

	if (a[k + 1] < a[k]) return megold(a, k + 1, j);
	
	if (a[k - 1] == 0) {
		cout << 0 << " " << k - 1 << endl;
		cin >> a[k - 1];
	}

	if (a[k - 1] > a[k] && a[k] < a[k + 1]) {
		cout << 1 << " " << k << endl;
		return 0;
	}

	if (a[k - 1] < a[k]) return megold(a, b, k - 1);
	
	return 0;
}

void input(int& n) {
	cin >> n;
}

int main() {
	int n;
	input(n);

	vector<uint64_t> a(n + 1, 0);
	a[1] = a[n] = UINT64_MAX;	
	megold(a, 1, n);

	return 0;
}