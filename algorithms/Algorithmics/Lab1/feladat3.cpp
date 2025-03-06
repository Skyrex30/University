/*
Kijelentés
Olvassunk be a billentyűzetről három 64-bites előjeles egész számot. Írjuk ki
őket növekvő sorrendben! Ne rendezzük a számokat, csak írjuk ki a kért sorrend-
ben. Egymás utáni elágazások helyett, egymásba ágyazottakat alkalmazzunk.
*/

#include <iostream>
#include <cstdint>

using namespace std;

int main() {
	int64_t a, b, c;
	cin >> a >> b >> c;
    if (a <= b) {
        if (b <= c) {
            cout << a << " " << b << " " << c << endl;
        }
        else {
            if (a <= c) {
                cout << a << " " << c << " " << b << endl;
            }
            else {
                cout << c << " " << a << " " << b << endl;
            }
        }
    }
    else {
        if (a <= c) {
            cout << b << " " << a << " " << c << endl;
        }
        else {
            if (b <= c) {
                cout << b << " " << c << " " << a << endl;
            }
            else {
                cout << c << " " << b << " " << a << endl;
            }
        }
    }

    return 0;
}