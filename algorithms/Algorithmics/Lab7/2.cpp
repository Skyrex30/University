/* Nagy Csongor, 513-s csoport
* Lab 7/02
Kijelentés
Adott egy 𝑛 elemű tömb, mely 64 bites valós számokat tartalmaz és egy 𝑘
természetes szám. Határozzuk meg a tömb 𝑘. legkisebb elemét, anélkül, hogy
rendeznénk azt, vagy használnánk az nth_element függvényt!
*/


#include <iostream>
#include <vector>

using namespace std;

int feloszt(vector<double>& nums, int start, int end) {
	int randomIndex = start + rand() % (end - start + 1);
	swap(nums[randomIndex], nums[end]);

	int i = start - 1, j;
	for (j = start; j < end; j++) {
		if (nums[j] < nums[end]) {
			i++;
			swap(nums[i], nums[j]);
		}
	}

	i++;
	swap(nums[i], nums[j]);
	return i;
}

void quickSort(vector<double> &a, int k, int b, int j) {
	int m;
	if (b < j) {
		m = feloszt(a,  b, j);
		if (k < m) {
			quickSort(a, k, b, m - 1);
		}
		else if (k > m) {
			quickSort(a, k, m + 1, j);
		}
		else return;
	} 

	return;
}

bool checkIfAscending(vector<double> a) {
	for (int i = 0; i < a.size() - 1; i++) {
		if (a[i] > a[i + 1]) {
			return false;
		}
	}

	return true;
}

bool checkIfDescending(vector<double> a) {
	for (int i = 0; i < a.size() - 1; i++) {
		if (a[i] < a[i + 1]) {
			return false;
		}
	}

	return true;
}

void input(int& n, int& k, vector<double> &a) {
	cin >> n >> k;

	for (int i = 0; i < n; i++) {
		double num;
		cin >> num;
		a.push_back(num);
	}
}

int main() {
	int n, k;
	vector<double> a;
	input(n, k, a);

	if (checkIfAscending(a)) { 
		cout << a[k - 1];
		return 0; 
	}
	if (checkIfDescending(a)) {
		cout << a[n - k]; 
		return 0;
	}

	srand(time(0));

	quickSort(a, k, 0, n - 1);

	cout << a[k - 1];

	return 0;
} 