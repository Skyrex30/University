/*
Kijelentés
Számítsuk ki hány szökőév volt/lesz két évszám között, mindkét évszámot
beleszámítva!
*/

#include <iostream>

using namespace std;

uint64_t szokoevek(uint64_t x) {
	uint64_t szokoev = (x / 4) - (x / 100) + (x / 400);

	return szokoev;
}

int main()
{
	// freopen("in.txt", "rt", stdin);
	// freopen("out.txt", "rt", stdout);

	uint64_t ev1 = 0, ev2 = 0;
	cin >> ev1 >> ev2;

	if (ev1 == 0 && ev2 == 0) {
		cout << 1;
		return 0;
	}
	uint64_t szokoevekSzama = 0;
	if (ev1 == 0) {
		szokoevekSzama = szokoevek(ev2) + 1;
	} else {
		szokoevekSzama = szokoevek(ev2) - szokoevek(ev1 - 1);
		}

	cout << szokoevekSzama;
	return 0;
}