/*
Nagy Csongor, 513-as csoport, 2023-24_Lab4/08

Kijelentés
Írjunk rekurzív algoritmust, amely átalakít egy, a 16-os számrendszerben
megadott egész számot a 10-es számrendszerbe! Ne használjunk ismétlő struk-
túrát a beolvasáson kívül!
*/

#include <iostream>

using namespace std;

uint64_t orosz(uint64_t a, uint64_t b, uint64_t c = 0) {
	if (a == 0) {
		return c;
	}
	else {
		c += (a % 2) * b;
		a /= 2;
		b *= 2;
		return orosz(a, b, c);
	}
}

int main() {
	uint64_t a, b;
	cin >> a >> b;
	cout << orosz(a, b) << endl;
	return 0;
}