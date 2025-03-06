/*
Kijelentés
Hasonlítsuk össze két egész típusú változó cseréjének sebességét három külön-
böző módszerrel:
• segédváltozóval
• összeadással és kivonással
• xor cserével
*/

#include <iostream>
#include <ctime>

using namespace std;

void csere_temp(int &a, int &b) {
	int temp = a;
	a = b;
	b = temp;
}

void csere_XOR(int &a, int &b) {
	a = a xor b;
	b = a xor b;
	a = a xor b;

}

void csere_osszegKivonas(int& a, int& b) {
	a = a + b;
	b = a - b;
	a = a - b;

}

int main() {

	int a, b;
	cin >> a >> b;

	int lefutasok_szama = 1000000;
	clock_t start_time, end_time;

	// Időmérés a segétváltozóval való cserére
	start_time = clock();
	for (int i = 0; i < lefutasok_szama; i++) {
		csere_temp(a, b);

	}
	end_time = clock();
	float time_taken = float(end_time - start_time) / CLOCKS_PER_SEC;
	
	cerr << time_taken << endl;

	// Időmérés az összeadás és kivonás módszerével való cserére

	start_time = clock();
	for (int i = 0; i < lefutasok_szama; i++) {
		csere_osszegKivonas(a, b);

	}
	end_time = clock();
	time_taken = float(end_time - start_time) / CLOCKS_PER_SEC;

	cerr << time_taken << endl;
	
	// Időmérés az XOR cserére

	start_time = clock();
	for (int i = 0; i < lefutasok_szama; i++) {
		csere_XOR(a, b);

	}
	end_time = clock();
	time_taken = float(end_time - start_time) / CLOCKS_PER_SEC;

	cerr << time_taken << endl;

	cout << b << " " << a;

	return 0;
}