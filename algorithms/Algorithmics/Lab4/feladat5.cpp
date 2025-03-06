/*
Nagy Csongor, 513-as csoport, 2023-24_Lab4/04

Kijelentés
Rendezzünk egy 𝑛 elemű tömböt számjegyes rendezéssel!
*/

#include <iostream>
#include <vector>
#include <fstream>



using namespace std;


void input(int n, vector<uint64_t>& num) {
	ifstream fin("input.txt");
	ofstream fout("output.txt");
	for (int i = 0; i < n; i++) {
		uint64_t numInput;
		fin >> numInput;
		num.push_back(numInput);
	}
}

void output(int n, vector<uint64_t> num) {
	ifstream fin("input.txt");
	ofstream fout("output.txt");
	for (int i = 0; i < n; i++) {
		fout << num[i] << endl;
	}
}

uint64_t searchMax(vector<uint64_t> num, int n) {
	uint64_t max = num[0];
	for (int i = 0; i < n - 1; i++) {
		if (num[i + 1] > max) {
			max = num[i + 1];
		}
	}
	return max;
}

void countSort(int n, vector<uint64_t>& num, uint64_t exp) {
	vector<uint64_t> output(n);
	int i;
	vector<uint64_t> count(10, 0);  // Initialized to 0 for all elements

	for (i = 0; i < n; i++) {
		count[(num[i] / exp) % 10]++;
	}

	for (i = 1; i < 10; i++) {
		count[i] += count[i - 1];
	}

	for (i = n - 1; i >= 0; i--) {
		output[count[(num[i] / exp) % 10] - 1] = num[i];
		count[(num[i] / exp) % 10]--;
	}
	for (i = 0; i < n; i++) {
		num[i] = output[i];
	}
}

int radixSort(int n, vector<uint64_t>& num) {
	uint64_t max = searchMax(num, n);
	for (uint64_t place = 1; max / place > 0; place *= 10) {
		countSort(n, num, place);
		if (place * 10 == 0) {
			return 0;
		}
	}
}

int main() {
	ifstream fin("input.txt");
	ofstream fout("output.txt");
	int n;
	fin >> n;
	vector<uint64_t> num;
	input(n, num);
	if (n == 1) {
		fout << num[0];
		return 0;
	}
	radixSort(n, num);
	output(n, num);

	return 0;
}