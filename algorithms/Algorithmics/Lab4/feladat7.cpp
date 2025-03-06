/*
Nagy Csongor, 513-as csoport, 2023-24_Lab4/07

Kijelentés
Írjunk rekurzív algoritmust, amely kiszámítja egy 𝑛-ed fokú polinom értékét
egy adott valós 𝑥 pontban! Ne használjunk ismétlő struktúrát a beolvasáson
kívül!
*/

#include <vector>
#include <iostream>

using namespace std;

void input(int n, vector<double>& polinom) {
	for (int i = 0; i <= n; i++) {
		double num;
		fin >> num;
		polinom.push_back(num);
	}
}

double calculatePolinomial(int n, vector<double>& polinom, double pow, int i, double x) {
	if (i >= 0) {
		return polinom[i] * pow + calculatePolinomial(n, polinom, pow * x, i - 1, x);
	}
	else return 0;
}


int main() {
	int n;
	double x;
	fin >> n;
	int i = n;
	vector<double> polinom;
	input(n, polinom);
	fin >> x;
	double result = calculatePolinomial(n, polinom, 1, i, x);
	fout << result;
}