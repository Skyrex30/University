/*
Nagy Csongor, 513-as csoport, 2023-24_Lab5/04

Kijelentés
Bontsunk fel egy szót minden lehetséges módon palindromszavakra!
*/

#include <iostream>
#include <vector>

using namespace std;

void kiir(vector<int> sum, int n, string input) {
	int i = 0, j = 0;
	while (i < n) {
		for (int k = i; k <= i + sum[j] - 1; k++) {
			cout << input[k];
		}
		cout << " ";
		i += sum[j];
		j++;
	}
	cout << endl;
	return;
}

bool isPalindrome(int k, int n, string s) {
	int i = k, j = n;
	while (i < j) {
		if (s[i] != s[j]) {
			return false;
		}
		i++;
		j--;
	}
	return true;
}

bool checkIfPalindrom(int k, string input, vector<int> sum) {
	bool ok = true;
	int i = 0, j = 0;
	while (i < k) {
		ok = isPalindrome(i, i + sum[j] - 1, input);
		if (!ok) return false;
		i += sum[j];
		j++;
	}
	return ok;
}

void searchForPalindrom(int n, string input, vector<int> sum, int k, int s) {
	for (int j = 1; j <= s; j++) {
		sum[k] = j;
		if (j < s) {
			if (checkIfPalindrom(k, input, sum) == true) {
				searchForPalindrom(n, input, sum, k + 1, s - j);
			}
			else return;
		}
		else if(checkIfPalindrom(n, input,sum) == true) {
			kiir(sum, n, input);
		}
	}
	return;
}

void beolvas(int& n, string& s, vector<int>& sum) {
	cin >> n;
	cin >> s;

	for (int i = 0; i < n; i++) {
		sum.push_back(0);
	}
}

int main() {
	int n;
	string s;
	vector<int> sum;
	
	beolvas(n, s, sum);

	searchForPalindrom(n, s, sum, 0, n);

	return 0;
}