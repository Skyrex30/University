#include <iostream>
#include <vector>
using namespace std;

void visszatesz(int n, vector<bool>& a);

bool megold(int n, vector<bool>& a) {
	if (n == 0) {
		a[0] = true;
		cout << 1 << " " << n + 1 << endl;
		return true;
	}
	if (n == 1 && a[n - 1] == false) {
		a[n] = true;
		cout << 1 << " " << n + 1 << endl;
		a[0] = true;
		cout << 1 << " " << 1 << endl;
		return true;
	}
	if (a[n - 1] == false && megold(n - 2, a) == true) {
		a[n] = true;
		cout << 1 << " " << n + 1 << endl;
		visszatesz(n - 2, a);
		return true;
	}
}

void visszatesz(int n, vector <bool>& a) {
	if (n == 0) {
		a[0] = false;
		cout << 2 << " " << n + 1 << endl;
		return;
	}
	if (a[n - 1] == true) {
		visszatesz(n - 1, a);
		a[n] = false;
		cout << 2 << " " << n + 1 << endl;
		return;
	}
}

int main() {
	int n;
	cin >> n;

	vector<bool> a(n, false);
	for (int i = 1; i < n; i++) {
		megold(n - i, a);
	}
	return 0;
}