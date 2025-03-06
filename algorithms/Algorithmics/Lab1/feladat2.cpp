/*
Kijelentés
Hasonlítsuk össze egy ismétlődő ciklus sebességét amely a beolvasott szám
gyökéig megy el, háromféleképpen implementálva:
• while (i*i <= n)
• sqrt(n)-t segédváltozóba mentve a ciklus előtt
• while (i <= sqrt(n))
Teszteljük a három változatot 𝑛 különböző értékeire 10 és 1 000 000 000
között.
*/

#include <iostream>
#include <ctime>
#include <cmath>
using namespace std;

void implementacio_1 (int& n, int &iteraciok_szama) {
	int i = 1;
	while (i * i <= n) {
		i++;
		iteraciok_szama++;
	}
}

void implementacio_2(int& n) {
	int temp = sqrt(n);
	int i = 1;
	while (i <= temp) {
		i++;
	}
}

void implementacio_3(int& n) {
	int i = 1;
	while (i <= sqrt(n)) {
		i++;
	}
}


int main() {

	int n, iteraciok_szama = 0;
	cin >> n;

	clock_t start_time, end_time;
	int lefutasok_szama = 1000000;
	// while (i*i <= n) módszerrel:

	start_time = clock();
	for (int i = 0; i < lefutasok_szama; i++) {
		implementacio_1(n, iteraciok_szama);

	}
	end_time = clock();

	float time_taken = float(end_time - start_time) / CLOCKS_PER_SEC;

	cerr << time_taken << endl;

	//segédváltozós módszerrel:

	start_time = clock();
	for (int i = 0; i < lefutasok_szama; i++) {
		implementacio_2(n);

	}
	end_time = clock();

	time_taken = float(end_time - start_time) / CLOCKS_PER_SEC;

	cerr << time_taken << endl;


	//while (i <= sqrt(n)) módszerrel:

	start_time = clock();
	for (int i = 0; i < lefutasok_szama; i++) {
		implementacio_3(n);

	}
	end_time = clock();

	time_taken = float(end_time - start_time) / CLOCKS_PER_SEC;

	cerr << time_taken << endl;

	cout << iteraciok_szama / 1000000;

	return 0;
}