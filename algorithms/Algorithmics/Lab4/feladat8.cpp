/*
Nagy Csongor, 513-as csoport, 2023-24_Lab4/08

Kijelentés
Írjunk rekurzív algoritmust, amely átalakít egy, a 16-os számrendszerben
megadott egész számot a 10-es számrendszerbe! Ne használjunk ismétlő struk-
túrát a beolvasáson kívül!
*/

#include <iostream>
#include <vector>

using namespace std;

void input(int n, vector<char> &numToConvert) {
	for (int i = 0;  i < n; i++) {
		char num;
		cin >> num;
		numToConvert.push_back(num);
	}
}

uint64_t convert(int n, vector<char>& numToConvert, int i, bool& ok, uint64_t converted = 0, uint64_t pow = 1) {
	int szj;
	if (i >= 0) {
		if ((numToConvert[i] >= '0' && numToConvert[i] <= '9' || (numToConvert[i] >= 'A' && numToConvert[i] <= 'F'))) {
			if (numToConvert[i] > '9') {
				szj = numToConvert[i] - '7';
			} else  szj = numToConvert[i] - '0';

			return convert(n, numToConvert, i - 1, ok, converted + szj * pow, pow * 16);
		}
		else {
			ok = false;
				return 0;
		}
	}
	else return converted;
}

int main() {
	int n;
	cin >> n;
	if (n > 16) {
		cout << -1 << endl;
		return 0;
	}
	int i = n - 1;
	bool ok = true;
	vector<char> numToConvert;
	input(n, numToConvert);
	uint64_t result = convert(n, numToConvert, i, ok);


	if (result > UINT64_MAX || ok == false) {
		cout << -1 << endl;
	} else cout << result;
}