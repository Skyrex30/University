/*
Nagy Csongor, info+, 2024-25_Lab2/03

Kijelentés
Adott egy római szám, írjuk ki arab számjegyekkel!
*/

#include <iostream>
#include <string>

using namespace std;

string input() {
	string s;
	getline(cin >> ws, s);

	return s;
}

int convertToArabic(string roman) {
	int sum = 0, last = 0;;
	for (int i = 0; i < roman.length(); i++) {
		int num = 0;
		if (roman[i] == 'M') num = 1000;
		if (roman[i] == 'D') num = 500;
		if (roman[i] == 'C') num = 100;
		if (roman[i] == 'L') num = 50;
		if (roman[i] == 'X') num = 10;
		if (roman[i] == 'V') num = 5;
		if (roman[i] == 'I') num = 1;
		if (num > last) sum -= 2 * last; // Kétszer vonjuk ki, mert egyszer már hozzáadtuk

		sum += num;
		last = num;
	}

	return sum;
}

void output(int num) {
	cout << "A megadott szam arab szamjegyekkel: " << num << endl;
}

int main() {
	string roman = input();
	output(convertToArabic(roman));
}