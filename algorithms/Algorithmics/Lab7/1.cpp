/*Nagy Csongor, 513 - as csoport.
* Lab7/1
* 
* Kijelentés
Legyen a következő játék: az egyik játékos (Játékos1) gondol egy 1 és 1000
közötti természetes számra. A másik játékosnak (Játékos2) ki kell találnia ezt
a számot minél kevesebb próbálgatással. A titkos szám „birtokosa” (Játékos1)
egy-egy találgatásra csak annyit válaszol, hogy a titkos szám kisebb vagy nagyobb
mint a másik játékos (Játékos2) által feltételezett szám.
Írjunk programot amely a fenti játékot szimulálja, úgy, hogy a programunk a
Játékos2, azaz ő találja ki a felhasználó titkos számát.
*/

#include <iostream>

using namespace std;

void tipp() {
	int n, answer;
	int i = 1, j = 1000;

	while (i <= j){

		n = (i + j) / 2;
		cout << n << endl;
		cin >> answer;
		if (answer == 0) return;
		if (answer == 1) {
			i = n + 1;
		}
		else j = n - 1;
	}

}

int main() {
	tipp();

	return 0;
}