/*
Kijelent�s
Sz�moljuk meg egy adott sz�vegben a mag�nhangz�k sz�m�t!
*/

#include <iostream>
#include <string>

using namespace std;

void beolvas(int& n, string& s) {
	cin >> n;
	getline(cin, s);
	getline(cin, s);
}

bool maganhangzo(char c) {
	string maganhangzok = ("aAeEiIoOuU");
	for (int i = 0; i < maganhangzok.length(); i++) {
		if (c == maganhangzok[i]) {
			return true;
		}
	}
	return false;
}

//Megszamlalas tetele
int maganhangzokSzama(string s) {
	int db = 0;
	for (int i = 0; i < s.length(); i++) {
		if (maganhangzo(s[i])){
			db++;
		}
	}
	return db;
}

int main() {
	int n, db = 0;
	string s;
	beolvas(n, s);
	db = maganhangzokSzama(s);

	cout << db << endl;

	return 0;
}