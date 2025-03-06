/*
Kijelentés
Adott a következő sorozat, amelynek minden elemét – az elsőt kivéve – az
előző elem segítségével generáljuk: 1, 11, 21, 1211, 111221, . . .
A generálási szabály a következő:
• megszámoljuk, balról jobbra haladva az előző érték számjegyeit
• az új értéket úgy kapjuk meg, hogy beírjuk a régi érték összes számjegyeinek
előfordulási számát és az illető számjegyet
*/

#include <iostream>
#include <string>

using namespace std;

string generateNext(string s) {
	string result = "";
	int n = s.length();
	int count = 1;

	for (int i = 0; i < n; i++) {
		if (i < n - 1 && s[i] == s[i + 1]) {
			count++;
		}
		else {
			result += to_string(count) + s[i];
			count = 1;
		}

	}
	return result;
}

string generateSequence(int n) {
	string s = "1";
	
	for (int i = 1; i < n; i++) {
		s = generateNext(s);
	}
	return s;
}

int main() {
	int n;
	cin >> n;

	string sequence = generateSequence(n);

	cout << sequence;

	return 0;
}