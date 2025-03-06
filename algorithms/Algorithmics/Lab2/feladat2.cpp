/*
Kijelentés
Határozzuk meg egy adott 𝑥 természetes szám legkisebb osztóját, amely
nagyobb mint 1!
*/

#include <iostream>
#include <cmath>

using namespace std;

int main() 
{
	unsigned long x = 0;
	cin >> x;

	for (int i = 2; i <= sqrt(x); i++) {
		if (x % i == 0) {
			cout << i;
			return 0;
		}
	}

	cout << x;

	return 0;
}