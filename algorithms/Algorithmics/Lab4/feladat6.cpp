/*
Nagy Csongor, 513-as csoport, 2023-24_Lab4/06

Kijelentés
Írjunk rekurzív algoritmust, amely kiír egy 𝑛 természetes számot törzstényezőkre
bontva! Ne használjunk egyáltalán ismétlő struktúrát!
*/

#include <iostream>
#include <vector>
#include <string>

using namespace std;

uint32_t primeFactors(uint32_t n, uint32_t divisor, int db, int i, uint32_t k) {
    if(n <= 1) {
        if (n == 1) cout << divisor;
        return 0;
    }                  

    if (n % divisor == 0) {
        primeFactors(n / divisor, divisor, db + 1, i, k);
    }
    else if (db > 1) {
        cout << divisor;
        cout << "^" << db << "*";
    }
    else if (db == 1) {
        cout << divisor << "*";
    }
    if (n % divisor != 0 && divisor == 2) return primeFactors(n, divisor + 1, 0, i, 1);  // Move to the next divisor
    else if (n % divisor != 0 && i == -1) return primeFactors(n, 6*k + i, 0, i * (-1), k);
    else if (n % divisor != 0 && i == 1)  return primeFactors(n, 6 * k + i, 0, i * (-1), k + 1);
    return 0;
}

int main() {
    uint32_t n, divisor = 2;
    cin >> n;
    if (n == 1) cout << 1;
    primeFactors(n, divisor, 0, -1, 1);
    return 0;
}