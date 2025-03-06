/*
Nagy Csongor, 513-as csoport, 2023-24_Lab3/06

Kijelentés
Keressük meg adott számmal bezárólag a legtöbb osztójú természetes számot
*/
#include <iostream>
#include <vector>
#include <cmath>

using namespace std;

bool isPrime(uint64_t n) {
    if (n <= 1)
        return false;
    if (n <= 3)
        return true;

    if (n % 2 == 0 || n % 3 == 0)
        return false;

    for (uint64_t i = 5; i * i <= n; i += 6)
        if (n % i == 0 || n % (i + 2) == 0)
            return false;

    return true;
}

int countDivisors(uint64_t num) {
    if (num <= 1)
        return 1;

    int count = 0;
    for (uint64_t i = 2; i < sqrt(num); i++) {
        if (num % i == 0) {
            if (num / i == i)
                count++;
            else
                count += 2;
        }
    }
    return count;
}

uint64_t findLowestNumberWithMaxDivisors(uint64_t range,int  &maxDivisorCount) {
    uint64_t numberWithMaxDivisors = 1;
    for (uint64_t i = 2; i <= range; i++) {
        if (!isPrime(i)) {
            int divisorCount = countDivisors(i);
            if (divisorCount > maxDivisorCount) {
                maxDivisorCount = divisorCount;
                numberWithMaxDivisors = i;
            }
        }
    }
    return numberWithMaxDivisors;
}

int main() {
    for (uint64_t range = 0; range < 100000000; range++) {
        int maxDivisorCount = 1;
        uint64_t result = findLowestNumberWithMaxDivisors(range, maxDivisorCount);
        cout << result << " " << maxDivisorCount << endl;
      
    }
    return 0;
}
