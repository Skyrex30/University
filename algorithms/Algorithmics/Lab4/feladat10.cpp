/*
Nagy Csongor, 513-as csoport, 2023-24_Lab4/08

Kijelentés
Írjunk rekurzív algoritmust, amely meghatározza egy adott 𝑎 szám né-
gyzetgyökét, felhasználva a következő sorozatot, melyről tudjuk, hogy √𝑎-hoz
konvergál.
*/

#include <iostream>
#include <iomanip>
#include <cmath>

using namespace std;

double calculateSqrt(double n, double sqrt_n, double xBefore, double x) {
	if (x - sqrt_n < 0.0000001) {
		return x;
	}
	double temp = x;
	x = (xBefore + (n/ xBefore)) / 2;
	xBefore = temp;
	return calculateSqrt(n, sqrt_n, xBefore, x);

}

int main() {
	double a, sqrt_a;
	cin >> a;
	sqrt_a = sqrt(a);
	cout << setprecision(6) << fixed << calculateSqrt(a, sqrt_a, 0, a);

	return 0;
}