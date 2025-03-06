/*

Kijelent�s
Adott egy csoport �v v�gi vizsgajegyeinek list�ja. T�r�lj�k ki az 5-�sn�l
kisebb jegyeket a list�b�l. A m�d�s�t�st a beolvasott t�mbben kell elv�gezni,
nem el�g csak ki�ratni az eredm�nyt.
*/

#include <iostream>
#include <fstream>

using namespace std;

ifstream fin("in.txt");
ofstream fout("out.txt");


void beolvas(int& n, double*& a) {
	for (int i = 0; i < n; i++) {
		fin >> a[i];
	}
}

void kiir(int n, double* a) {
	for (int i = 0; i < n; i++) {
		fout << a[i] << ' ';
	}
}

//Kivalogatas tetele:
void kivalogat(int& n, double*& a) {
	int i = 0;
	while (i < n) {
		if (a[i] < 5) {
			a[i] = a[n - 1];
			n = n - 1;
		}
		else {
			i++;
		}
	}
}

int main() {
	int n;
	fin >> n;
	double* a = new double[n];
	beolvas(n, a);

	kivalogat(n, a);

	kiir(n, a);

	delete[] a;

	return 0;
}