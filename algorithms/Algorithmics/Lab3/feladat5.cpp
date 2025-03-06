/*
Nagy Csongor, info+, 2024-25_Lab2/05

Kijelentés
Bontsunk törzstényezőkre egy adott 𝑛 természetes számot!
*/

#include <iostream>
#include <vector>
#include <cmath>

using namespace std;

bool prim_e(uint64_t n) {
	bool prim = false;
	int oszto = 3;
	uint64_t sqrt_n = 0;
	if (n == 1) {
		prim = false;
	}
	else {
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
					sqrt_n= sqrt(n);
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

uint64_t input() {
	uint64_t n;
	cin >> n;
	return n;
}

void output(vector<uint64_t> tenyezo, vector<int> kitevo, int m, uint64_t n) {
	if (n > 1) {
		if (kitevo[0] == 1) {
			cout << tenyezo[0];
		}
		else {
			cout << tenyezo[0] << '^' << kitevo[0];
		}
		for (int i = 1; i <= m; i++) {
			if (i < tenyezo.size() && i < kitevo.size()) {
				if (kitevo[i] == 1) {
					cout << '*' << tenyezo[i];
				}
				else {
					cout << '*' << tenyezo[i] << '^' << kitevo[i];
				}
			}
		}
	}
	else {
		cout << n;
	}
}


void felbont(uint64_t n, int &m, vector<uint64_t>& tenyezo, vector<int>& kitevo) {
	m = 0;
	uint64_t oszto = 2;
	while ((n != 1) && (prim_e(n) == false)) {
		int hatvany = 0;
		while (n % oszto == 0) {
			hatvany++;
			n = n / oszto;
		}
		if (hatvany != 0) {
			m++;
			tenyezo.push_back(oszto);
			kitevo.push_back(hatvany);
		}
		oszto++;
	}
	if (n > 1) {
		m++;
		tenyezo.push_back(n);
		kitevo.push_back(1);
	}
}

int main() {
	uint64_t n = input();
	if (n == 1) return 0;
	int m = 0;
	vector<uint64_t> tenyezo;
	vector<int> kitevo;
	felbont(n, m, tenyezo, kitevo);
	output(tenyezo, kitevo, m, n);

	return 0;
}

