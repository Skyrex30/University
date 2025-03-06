/*
Nagy Csongor, info+, 2024-25_Lab2/04

Kijelentés
Adott egy 𝑛 arab szám, írjuk ki római számjegyekkel!
*/
#include <iostream>

using namespace std;

string arabicToRoman(int arabicNumber) {
    string romanNumeral = "";
    while (arabicNumber >= 1000) {
        romanNumeral += "M";
        arabicNumber -= 1000;
    }

    while (arabicNumber >= 900) {
        romanNumeral += "CM";
        arabicNumber -= 900;
    }

    while (arabicNumber >= 500) {
        romanNumeral += "D";
        arabicNumber -= 500;
    }

    while (arabicNumber >= 400) {
        romanNumeral += "CD";
        arabicNumber -= 400;
    }

    while (arabicNumber >= 100) {
        romanNumeral += "C";
        arabicNumber -= 100;
    }

    while (arabicNumber >= 90) {
        romanNumeral += "XC";
        arabicNumber -= 90;
    }

    while (arabicNumber >= 50) {
        romanNumeral += "L";
        arabicNumber -= 50;
    }

    while (arabicNumber >= 40) {
        romanNumeral += "XL";
        arabicNumber -= 40;
    }

    while (arabicNumber >= 10) {
        romanNumeral += "X";
        arabicNumber -= 10;
    }

    while (arabicNumber >= 9) {
        romanNumeral += "IX";
        arabicNumber -= 9;
    }

    while (arabicNumber >= 5) {
        romanNumeral += "V";
        arabicNumber -= 5;
    }

    while (arabicNumber >= 4) {
        romanNumeral += "IV";
        arabicNumber -= 4;
    }

    while (arabicNumber >= 1) {
        romanNumeral += "I";
        arabicNumber -= 1;
    }

    return romanNumeral;
}

int main() {
    int arabicNumber;
    cin >> arabicNumber;
    string romanNumeral = arabicToRoman(arabicNumber);

    cout << romanNumeral << endl;

    return 0;
}
