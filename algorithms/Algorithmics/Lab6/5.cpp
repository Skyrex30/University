/*
Nagy Csongor, 513-as csoport, 2023-24_Lab6/05

Kijelentés
Adva van 𝑛 + 1 cipősdoboz és 𝑛 pár cipő, amelyek meg vannak számozva 1-től
𝑛-ig. Az 𝑛 pár cipő 𝑛 + 1 dobozban található, a dobozok közül az egyik üres. El
kell rendezni a cipőket úgy, hogy minden cipő a saját dobozába kerüljön és az üres
doboz a sor végére. Munka közben csak egy pár cipőt szabad kivenni a dobozból,
amelyben található, és azonnal be kell tenni az üres dobozba. Állapítsuk meg a
költöztetések sorozatát minimális számú művelettel.
*/
#include <iostream>
#include <vector>
#include <fstream>

using namespace std;

void input(int &n, vector<int> &arr) {
	ifstream fin("test5.txt");
	fin >> n;

	for (int i = 0; i <= n; i++) {
		int num;
		fin >> num;
		arr.push_back(num);
	}
}

bool checkIfCorrect(int n, vector<int> arr) {
	for (int i = 0; i < n; i++) {
		if (arr[i] != i + 1) return false;
	}
	return true;
}

void output(int db, vector<pair<int, int>> moves) {
	ofstream fout("test5out.txt");
	fout << db << endl;
	for (int i = 0; i < db; i++) {
		fout << moves[i].first << " " << moves[i].second << endl;
	}
}

void solve(int n, vector<int>& arr, int &db, vector<pair<int, int>> &moves) {
	
	while (!checkIfCorrect(n, arr)) {
		if (arr[n] != 0) {
			int i = 0;
			while (arr[i] != 0) i++;
			for (int j = 0; j <= n; j++) {
				if (arr[j] == i + 1) {
					arr[i] = i + 1;
					arr[j] = 0;
					db++;
					moves.push_back(make_pair(j + 1, i + 1));
					break;
				}
			}
		}
		else {
			for (int i = 0; i <= n; i++) {
				if (arr[i] != i + 1) {
					arr[n] = arr[i];
					arr[i] = 0;
					db++;
					moves.push_back(make_pair(i + 1, n + 1));
					break;
				}
			}
		}
	}
}

int main() {
	int n, db = 0;
	vector<int> arr;
	vector<pair<int, int>> moves;
	input(n, arr);

	solve(n, arr, db, moves);

	output(db, moves);

	return 0;
}