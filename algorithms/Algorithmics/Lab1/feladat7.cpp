/*
Kijelentés
Írjunk algoritmust az 𝑎𝑥2+𝑏𝑥+𝑐 = 0 valós együtthatójú egyenlet megoldására!
Az együtthatók 128-bites valós számok (long double). Garantált, hogy a 𝑏2−4𝑎𝑐
kifejezés és annak minden részkifejezése is ábrázolható 128-bites valós számként.
*/

#include <iostream>
#include <cmath>
#include <iomanip>

using namespace std;

long double del(long double a, long double b, long double c) {
	long double delta = (b * b) - (4 * a * c);

	return delta;
}

int main() {
	long double a = 0, b = 0, c = 0, delta = 0;
	cin >> a >> b >> c;

	delta = del(a, b, c);
	
	if (a == 0) {
		cerr << "Csak egy gyok van mivel a = 0";
		return 0;
	}

	if (delta == 0) {
		cerr << "Csak egy gyok van mivel a delta 0";
		return 0;
	}
	else {

		if (delta < 0) {
			cerr << "Nincs gyok mivel delta negativ szam";
			return 0;
		}
		else
		{
			long double gyok1 = 0, gyok2 = 0;
			gyok1 = (- b + sqrt(delta)) / (2 * a);
			gyok2 = (- b - sqrt(delta)) / (2 * a);

			if (gyok1 > gyok2) {
				cout << setprecision(1) << fixed << gyok2 << " " << gyok1 << endl;
			}
			else {
				cout << setprecision(1) << fixed << gyok1 << " " << gyok2 << endl;
			}
		}
	} 
	

	return 0;
}