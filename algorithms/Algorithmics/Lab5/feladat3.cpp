/*
Nagy Csongor, 513-as csoport, 2023-24_Lab5/03

Kijelentés
Adva van egy 𝑛 darab egész számot tartalmazó sorozat. Helyezzünk az
adott számok közé 𝑛 − 1 aritmetikai műveleti jelet a (+, −, *, /) halmazból, úgy,
hogy a kifejezés értéke legyen egyenlő egy adott 𝑥 számmal! A kifejezés értéke
minden lépésben legyen egész szám! A műveleteket nem a matematikából ismert
operátorprecedencia szerint végezzük, hanem megjelenésük sorrendjében.
*/

#include <iostream>
#include <vector>

using namespace std;


void kiir(vector<long> szamok, int n, vector <char> muveletek, long sumToCheck) {
	cout << sumToCheck << "=";
	for (int i = 0; i < n; i++) {
			cout << szamok[i] << muveletek[i];
	}
	cout << endl;
	return;
}

void muvel(int n, vector <long>& szamok, vector <char>& muveletek, int k, long sumToCheck, long sum, bool &kiirte) {
	if (sum == sumToCheck && k == n - 1) {
		kiir(szamok, n, muveletek, sumToCheck);
		kiirte = true;
		return;
	}
	else if (k == n - 1) {
		return;
	}
	muveletek[k] = '+';
	muvel(n, szamok, muveletek, k + 1, sumToCheck, sum + szamok[k + 1], kiirte);

	muveletek[k] = '-';
	muvel(n, szamok, muveletek, k + 1, sumToCheck, sum - szamok[k + 1], kiirte);

	muveletek[k] = '*';
	muvel(n, szamok, muveletek, k + 1,  sumToCheck, sum * szamok[k + 1], kiirte);

	if (sum % szamok[k + 1] == 0) {
		muveletek[k] = '/';
		muvel(n, szamok, muveletek, k + 1, sumToCheck, sum / szamok[k + 1], kiirte);
	}

	if (k == 0 && (muveletek[0] == '/' || muveletek[0] == '*') && kiirte == false) {
		cout << -1;
		return;
	}
		
}

void beolvas(int &n, vector<long> &x, vector<char> &muveletek, long& sum) {
	cin >> n;
	for (int i = 0; i < n; i++) {
		long num;
		cin >> num;
		x.push_back(num);
	}
	cin >> sum;

	for (int i = 0; i < n; i++) {
		muveletek.push_back(' ');
	}
}

int main() {
	int n;
	vector<long> szamok;
	vector<char> muveletek;
	long sum;
	beolvas(n, szamok, muveletek, sum);

	bool kiirte = false;
	muvel(n, szamok, muveletek, 0, sum, szamok[0], kiirte);

	return 0;
}