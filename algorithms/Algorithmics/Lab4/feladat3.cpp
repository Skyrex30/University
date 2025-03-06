/*
Nagy Csongor, 513-as csoport, 2023-24_Lab4/01

Kijelentés
Rendezzünk egy 𝑛 elemű tömböt minimumrendezéssel!
*/

#include <iostream>
#include <vector>

using namespace std;


void input(int n, vector<int64_t>& num) {
	for (int i = 0; i < n; i++) {
		uint64_t numInput;
		cin >> numInput;
		num.push_back(numInput);
	}
}

void output(int n, vector<int64_t>& num) {
	for (int i = 0; i < n; i++) {
		cout << num[i] << endl;
	}
}

void insertSort(int n, vector<int64_t> &num) {
	for (int j = 1; j < n; j++) {
		int64_t segéd = num[j];
		int64_t i = j - 1;

		while ((i >= 0) && (num[i] > segéd)) {
			num[i + 1] = num[i];
			i = i - 1;
		}
		num[i + 1] = segéd;
	}

}

int main() {
	int n;
	cin >> n;
	vector<int64_t> num;
	input(n, num);
	insertSort(n, num);
	output(n, num);

	return 0;
}