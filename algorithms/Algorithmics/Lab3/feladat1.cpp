/*
Nagy Csongor, info+, 2024-25_Lab2/01

Kijelentés
Bontsunk fel egy adott számot 2 hatványainak összegére!
*/

#include <iostream>

using namespace std;

void kiir(int a[], int i) {
	bool ok = 0;
	while (i != -1){
		if (a[i] == 1) {
			if (ok != 0) {
				cout << "+";
			}
			cout << "2^";
			cout << i;
			ok = 1;
		}
	i--;
	}
}

int main() {
	uint64_t n;
	cin >> n;

	int a[1000];

	int i = 0;
	while (n != 0) {
		a[i] = n % 2;
		n = n / 2;
		i++;
	}

	kiir(a, i);
	return 0;
}