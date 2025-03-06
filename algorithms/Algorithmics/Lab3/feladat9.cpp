/*
Nagy Csongor, info+, 2024-25_Lab2/09

Kijelentés
Határozzuk meg két szám különbségét!
*/

#include <iostream>
#include <string>
#include <fstream>

using namespace std;



void input(string &num1_str, string &num2_str, int &length1, int &length2) {
    cin >> length1 >> num1_str >> length2 >> num2_str;
}
void output(string result, bool isNegative) {
    if (isNegative == true) cout << "-" << result << endl;
    else cout << result << endl;
}

string calculateDifference(string num1_str, string num2_str, int length1, int length2, bool& isNegative) {
    int max_length = 0;
    if (length1 > length2) {
        max_length = length1;
    }
    else {
        max_length = length2;
    }

    while (length1 < max_length) {
        num1_str = '0' + num1_str;
        length1++;
    }

    while (length2 < max_length) {
        num2_str = '0' + num2_str;
        length2++;
    }

    string result;
    int borrow = 0;
    isNegative = false;

    if (num1_str < num2_str) {
        string temp = num1_str;
        num1_str = num2_str;
        num2_str = temp;
        isNegative = true;
    }
    for (int i = max_length - 1; i >= 0; --i) {
        int digit1 = num1_str[i] - '0';
        int digit2 = num2_str[i] - '0';

        int diff = digit1 - digit2 - borrow;

        if (diff < 0) {
            diff += 10;
            borrow = 1;
        } else {
            borrow = 0;
        }

        result = char(diff % 10 + '0') + result;
    }
    // Abban az esetben, ha az egyik szám nagyobb mint a másik kikell törölni a 0-kat az elejéről
        int nonZeroIndex = 0;
    while (nonZeroIndex < result.length() && result[nonZeroIndex] == '0') {
        nonZeroIndex++;
    }
    if (nonZeroIndex == result.length()) {
        result = "0";
    }
    else {
        for (int i = 0; i < result.length() - nonZeroIndex; ++i) {
            result[i] = result[i + nonZeroIndex];   //a nem 0 számokat az elejére visszük
        }
        result.resize(result.length() - nonZeroIndex); // a vector másik felét kitöröljük, ahol csak 0-k vannak
    }

    return result;
}

int main() {
  
    string num1_str, num2_str;
    int length1, length2;
    bool isNegative = false;
    input(num1_str, num2_str, length1, length2);
    string result = calculateDifference(num1_str, num2_str, length1, length2, isNegative);
    output(result, isNegative);

    return 0;
}
