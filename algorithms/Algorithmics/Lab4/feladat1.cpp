/*
Nagy Csongor, 513-as csoport, 2023-24_Lab4/01

Kijelentés
Rendezzünk egy 𝑛 elemű tömböt koktélrendezéssel!
*/

#include <iostream>
#include <vector>
#include <fstream>



using namespace std;

void input(int n, vector<int64_t>& num) {
	ifstream finput("input.txt");
	ofstream foutput("output.txt");
	for (int i = 0; i < n; i++) {
		int64_t numInput = 0;
		finput >> numInput;
		num.push_back(numInput);
	}
}

void output(int n, vector<int64_t> num) {
	ifstream finput("input.txt");
	ofstream foutput("output.txt");
	for (int i = 0; i < n; i++) {
		foutput << num[i] << endl;
	}
}

void koktelrendez(int n, vector<int64_t> &num) {
	int oldLeft = 0;
	int oldRight = n - 1;
	bool rendben = true;

	do {
		rendben = true;
		int right = 0;
		int left = 0;
		for (int i = oldLeft; i < oldRight; i++) {
			if (num[i] > num[i + 1]) {
				int64_t temp = num[i + 1];
				num[i + 1] = num[i];
				num[i] = temp;
				rendben = false;	
				right = i;
			}
		}
		if (!rendben) {
			oldRight = right;
			rendben = true;
			left = n;
			for (int i = oldRight; i >= oldLeft; i--) {
				if (num[i] > num[i + 1]) {
					int64_t temp = num[i + 1];
					num[i + 1] = num[i];
					num[i] = temp;
					rendben = false;
					left = i;
				}
			}
			oldLeft = left;
		}
	} while (!rendben);
}

int main() {
	ifstream finput("input.txt");
	ofstream foutput("output.txt");

	int n;
	finput >> n;
 	vector<int64_t> num;
	input(n, num);
	koktelrendez(n, num);
	output(n, num);
	
	return 0;
}