#include <iostream>
#include <vector>
#include <cmath>

using namespace std;

vector<int> getDivisors(int num) {
    vector<int> divisors;
    for (int i = 1; i <= sqrt(num); ++i) {
        if (num % i == 0) {
            divisors.push_back(i);
            if (i != num / i) {
                divisors.push_back(num / i);
            }
        }
    }
    return divisors;
}

void findDivisors(int n, vector<int>& numbers, int currentIndex, vector<vector<int>>& result) {
    if (currentIndex == n) {
        return;
    }

    int currentNumber = numbers[currentIndex];
    result.push_back(getDivisors(currentNumber));

    findDivisors(n, numbers, currentIndex + 1, result);
}

int main() {
    int n;
    cin >> n;

    vector<int> numbers(n);
    for (int i = 0; i < n; ++i) {
        cin >> numbers[i];
    }

    vector<vector<int>> divisorsList;
    findDivisors(n, numbers, 0, divisorsList);

    for (const auto& divisors : divisorsList) {
        cout << divisors.size() << endl;
        for (int divisor : divisors) {
            cout << divisor << " ";
        }
        cout << endl;
    }

    return 0;
}
