/*
Nagy Csongor, info+, 2024-25_Lab2/02

Kijelentés
Írjunk algoritmust, amely egy adott 𝑥 páros számot felír két különböző
prímszám összegeként (Goldbach-sejtés).
*/

#include <iostream>
#include <cmath>

using namespace std;

bool prim_e(int n) {
	bool prim = false;
	int oszto = 3, sqrt_n = 0;
	if (n == 1) {
		prim = false;
	}
	else{
		if (n % 2 == 0) {
			prim = (n == 2);
		}
		else if (n % 3 == 0 && n != 3)
		{
			return false;
		}
		else {
			if (n < 5) {
				prim = true;
			}
			else {
				if (((n - 1) % 6 != 0) && ((n + 1) % 6 != 0)) {
					prim = false;
				}
				else {
					prim = true;
					sqrt_n = sqrt(n);
					while (prim && (oszto <= sqrt_n)) {
						if (n % oszto == 0) {
							prim = false;
						}
						else {
							oszto = oszto + 2;
						}
					}
				}
			}
		}
	}
	return prim;
}


int main() {
	uint32_t x = 2, i = 2;
	cin >> x;
	while (i < x / 2) {
		if (prim_e(i)) {
			if (prim_e(x - i)) {
				cout << x << " = " << i << " + " << x - i << endl;
				return 0;
			}
		}
		if (i == 2) i++;
		else {
			i = i + 2;
		}
	}

	return 0;
}