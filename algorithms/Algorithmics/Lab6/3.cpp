/*
Nagy Csongor, 513-as csoport, 2023-24_Lab6/03

Kijelentés
Adott egy 𝑛 egész szám. Határozzuk meg a legkisebb olyan 𝑚 > 0 természetes
számot, amelyre 𝑚 számjegyeinek szorzata egyenlő 𝑛-el!
*/


#include <iostream>

using namespace std;

long long find_smallest_m(int n) {
    if (n == 0) {
        return 10; 
    }
    else if (n < 10) {
        return n;
    }
   
    int count[10] = { 0 };

    for (int digit = 9; digit > 1; --digit) {
        while (n % digit == 0) {
            count[digit]++;
            n /= digit;
        }
    }

    if (n > 1) {
        return -1;  
    }
    else {

        long long m = 0;
        for (int digit = 2; digit <= 9; ++digit) {
            for (int i = 0; i < count[digit]; ++i) {
                m = m * 10 + digit;
            }
        }

        return m;
    }
}

void input(int& n) {
    cin >> n;
}

int main() {
    int n;
    input(n);

    long long result = find_smallest_m(n);
    cout << result << endl;

    return 0;
}
