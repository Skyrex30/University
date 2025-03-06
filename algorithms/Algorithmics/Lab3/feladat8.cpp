/*
Nagy Csongor, info+, 2024-25_Lab3/08

Kijelentés
Határozzuk meg 𝑥 és 𝑦 szorzatát!
*/

#include <iostream>
#include <string>

using namespace std;

void input(string& num_str, long& multiplier, int& length) {
    cin >> length >> num_str >> multiplier;
}

void output(string result) {
    cout << result << endl;
}

string calculateProduct(string num_str, long multiplier, int length) {
    if (multiplier == 0) return "0";
    string result = "";
    uint64_t carry = 0;

    for (int i = length - 1; i >= 0; --i) {
        int digit = num_str[i] - '0';

        uint64_t product = digit * multiplier + carry;
        carry = product / 10;

        result = char(product % 10 + '0') + result;
    }

    while (carry > 0) {
        result = char(carry % 10 + '0') + result;
        carry /= 10;
    }

    return result;
}



int main() {
    string num_str;
    long multiplier;
    int length;

    input(num_str, multiplier, length);
    string result = calculateProduct(num_str, multiplier, length);
    output(result);

    return 0;
}