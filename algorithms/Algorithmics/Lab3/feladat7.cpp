/*
Nagy Csongor, info+, 2024-25_Lab2/07

Kijelentés
Határozzuk meg két szám összegét!
*/

#include <iostream>
#include <string>

using namespace std;

void input(string &num1_str, string &num2_str, int &length1, int &length2) {
    cin >> length1 >> num1_str >> length2 >> num2_str;
}
void output(string result) {
    cout << result << endl;
}

string calculateSum(string num1_str, string num2_str, int length1, int length2) {
    int max_length = 0;
    if (length1 > length2) {
        max_length = length1;
    }
    else {
        max_length = length2;
    }

    while (num1_str.length() < max_length) {
        num1_str = '0' + num1_str;
    }

    while (num2_str.length() < max_length) {
        num2_str = '0' + num2_str;
    }

    string result;
    int carry = 0;

    for (int i = max_length - 1; i >= 0; --i) {
        int digit1 = num1_str[i] - '0';
        int digit2 = num2_str[i] - '0';

        int sum = digit1 + digit2 + carry;
        carry = sum / 10;

        result = char(sum % 10 + '0') + result;
    }

    if (carry > 0) {
        result = char(carry + '0') + result;
    }

    return result;
}

int main() {
  
    string num1_str, num2_str;
    int length1, length2;
    input(num1_str, num2_str, length1, length2);
    string result = calculateSum(num1_str, num2_str, length1, length2);
    output(result);

    return 0;
}
