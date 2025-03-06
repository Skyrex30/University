/*
Kijelentés
Ismerjük egy osztály tanulóinak neveit (családnév + keresztnév) és év végi
átlagait. Állapítsuk meg, hogy egy adott nevű tanuló az első három díjazott
között van-e?
*/

#include <iostream>

using namespace std;

#ifdef SAJAT_GEP
freopen("in.txt", "r", stdin);
freopen("out.txt", "w", stdout);
#endif

void beolvas(int n, string*& names, double*& averages) {
	for (int i = 0; i < n; i++) {
		int length;
		string firstName, lastName;
		cin >> length >> firstName >> lastName;
		names[i] = firstName + " " + lastName;
		cin >> averages[i];
	}

}
double getAverage(string wantedStudent, int n, string* names, double* averages) {
	for (int i = 0; i < n; i++) {
		if (wantedStudent == names[i]) {
			return averages[i];
		}
	}
	cout << "nem" << endl;
	return -1;
}

int isInTopThree(int n, double avgOfWantedStudent, double* averages) {
	int db = 0;
	for (int i = 0; i < n; i++) {
		if (avgOfWantedStudent < averages[i]) {
			db++;
		}
	
		if (db == 3) {
			cout << "nem" << endl;
			return 0;
		}
	}
	cout << "igen" << endl;
	return 0;
}

int main() {
	int n, length;
	cin >> n;
	string* names = new string[n];
	double* averages = new double[n];
	beolvas(n, names, averages);
	
	string wantedStudent, firstName, lastName;
	cin >> length >> firstName >> lastName;
	wantedStudent = firstName + " " + lastName;

	double avgOfWantedStudent = getAverage(wantedStudent, n, names, averages);
	if (avgOfWantedStudent == -1) return 0;
	isInTopThree(n, avgOfWantedStudent, averages);

	delete[] names;
	delete[] averages;

	return 0;
}