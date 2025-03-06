/*
Adott három 64-bites valós szám (double): 𝑎, 𝑏, 𝑐. Képezhetik-e ezek
a számok egy háromszög oldalait? Ha igen, határozzuk meg és írjuk ki a
háromszögbe írt, illetve a háromszög köré írt kör sugarát! Ha nem, írjunk ki
-1-et!
A hárömszögbe írt és háromszög köré írt körök sugarai és az ezek kiszámításához
szükséges kifejezések minden részkifejezése ábrázolható 64-bites valósként
*/

#include <iostream>
#include <cmath>
#include <iomanip>

using namespace std;
const int MINUS_ONE = -1;

int main() {

	double a, b, c;
	cin >> a >> b >> c;

	if (a + b <= c || a + c <= b || b + c <= a) {
		cout << MINUS_ONE;
		return 0;
	}
	double s = (a + b + c) / 2;

	double bele_irt_sugar = sqrt((s - a) * (s - b) * (s - c) / s);

	double ter = sqrt(s * (s - a) * (s - b) * (s - c));
	
	double kore_irt_sugar = (a * b * c) / (4 * ter);

	cout << setprecision(3) << fixed << bele_irt_sugar << " " << kore_irt_sugar << endl;

	return 0;
}