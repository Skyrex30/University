/*
Kijelentés
Számítsuk ki egy háromszög területét, ha ismerjük oldalainak a hosszát!
Az oldalak hosszai 64-bites valós számok (double). A háromszög területe és
a számítások során megjelenő részkifejezések mindegyike ábrázolható 64-bites
valósként.
*/

#include <iostream>
#include <cmath>
#include <iomanip>

using namespace std;

int main() {

	double a, b, c;
	cin >> a >> b >> c;

	double s = (a + b + c) / 2;
	double ter = sqrt(s * (s - a) * (s - b) * (s - c));

	cout << setprecision(2) << fixed << ter;
	return 0;
}
