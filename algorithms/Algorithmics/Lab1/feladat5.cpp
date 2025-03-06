/*
Kijelentés
Számítsuk ki, adott 𝑛 és 𝑥 értékekre, a következő összeget:
𝑆 = 1 − 𝑥^2/2! + 𝑥^4/4! − 𝑥^6/6! + . . . + (−1)^𝑛 * 𝑥^(2𝑛)/(2𝑛)!
Megkötések:
• 1 ≤ 𝑛 ≤ 100
• 𝑥 64-bites valós szám (double)
• Minden részkifejezés ábrázolható 64-bites egészként
*/

#include <iostream>
#include <iomanip>

using namespace std;

int factorial(int n) {
	int result = 1;

	for (int i = 1; i <= n; i++) {
		result *= i;
	}

	return result;
}

double power(double x, int t) {

	if (t == 0)
		return 1.0;

	double result = 1.0;
	
	while (t > 0) {
		if (t % 2 == 1) {
			result *= x;
		}
		x *= x;
		t /= 2;
	}

	return result;
}

int main() {
	int n;
	double x;
	cin >> n >> x;
	
	double s = 1.0;
	double tag = 1.0;
	int egy = -1;

	for (int i = 1; i <= n; i++) {
		tag = power(egy, i) * power(x, 2 * i) / factorial(2 * i);
		s += tag;
	}
	cout << setprecision(2) << fixed << s << endl;
	return 0;
}