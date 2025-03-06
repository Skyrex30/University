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

void minimumRendez(int n, vector<int64_t> &num) {
	for (int i = 0; i < n - 1; i++) {
		int ind_min = i;
		for (int j = i + 1; j < n; j++) {
			if (num[ind_min] > num[j]) ind_min = j;
		}
		swap(num[i], num[ind_min]);
	}

}

int main() {
	int n;
	cin >> n;
	vector<int64_t> num;
	input(n, num);
	minimumRendez(n, num);
	output(n, num);

	return 0;
}