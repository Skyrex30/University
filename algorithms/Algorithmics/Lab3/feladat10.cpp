/*
Nagy Csongor, info+, 2024-25_Lab2/10

Határozzuk meg, hogy melyik számjegyet hányszor kell leírnunk, ha egyenként
minden természetes számot le akarunk írni az [𝑥, 𝑦] intervallumból!
*/

#include <iostream>
#include <vector>

using namespace std;

void countDigitsInRange(long long x, long long y, vector<long long>& digitCounts) {
    for (long long num = x; num <= y; ++num) {
        long long current = num;
        while (current > 0) {
            int digit = current % 10;
            digitCounts[digit]++;
            current /= 10;
        }
        if (num == 0) {
            digitCounts[0]++;
        }
    }
}

int main() {
    long long x, y;
    cin >> x >> y;

    vector<long long> digitCounts(10, 0);
    countDigitsInRange(x, y, digitCounts);

    for (long long count : digitCounts) {
        cout << count << " ";
    }
    cout << endl;

    return 0;
}
