/*
Nagy Csongor, 513-as csoport, 2023-24_Lab4/04

Kijelentés
Rendezzünk egy 𝑛 elemű tömböt a leszámláló rendezés azon változatával,
melyre nem értelmezhető, hogy stabil-e vagy sem!
*/

#include <iostream>
#include <vector>
#include <fstream>


using namespace std;


void input(int n, vector<int64_t>& num) {
	ifstream fin("input.txt");
	for (int i = 0; i < n; i++) {
		int64_t numInput;
		fin >> numInput;
		num.push_back(numInput);
	}
}

void output(int n, vector<int64_t>& num) {
	ofstream fout("output.txt");
	for (int i = 0; i < n; i++) {
		fout << num[i] << endl;
	}
}

int searchMax(vector<int64_t> num, int n) {
	int64_t max = num[0];
	for (int i = 0; i < n - 1; i++) {
		if (num[i + 1] > max) {
			max = num[i + 1];
		}
	}
	return max;
}

void countSort(int n, vector<int64_t>& num, int64_t k) {
	k = searchMax(num, n) + 1;
	vector<int64_t> darab(k, 0);
	for (int i = 0; i < n; i++) {
		darab[num[i]]++;
	}
	int q = 0;
	for (int i = 0; i < k; i++) {
		for (int j = 0; j < darab[i]; j++) {
			num[q] = i;
			q++;
		}
	}
}

int main() {
	ifstream fin("input.txt");
	ofstream fout("output.txt");
	int n;
	int64_t k = 0;
	fin >> n;
	vector<int64_t> num;
	input(n, num);
	if (n == 1) {
		fout << num[0];
		return 0;
	}
	countSort(n, num, k);
	output(n, num);

	return 0;
}