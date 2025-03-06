/*
Nagy Csongor, 513-as csoport, 2023-24_Lab6/02

Kijelentés
Egy falu főútcáján 𝑛 lakos él, minden két egymás melletti ház között
ugyanakkora a távolság. A falu jó bortermő vidéken található, így a lakók
szívesen fogyasztanak, de adnak is el bort. Érdekes módon a falun belül a
kereslet és a kínálat mindig egyenlő. Tudván, hogy két egymás melletti ház
között szállítani egy liter bort mindig egy egységnyi munkába kerül és ismervén,
hogy melyik lakos hány liter bort szeretne eladni vagy vásárolni, határozzuk
meg a minimális munkamennyiséget, amely szükséges ahhoz, hogy minden lakos
igénye ki legyen elégítve.
*/

#include <iostream>
#include <vector>
#include <cmath>
#include <fstream>


using namespace std;

void beolvas(int &n, vector<int> &hazak) {
	ifstream fin("test2.txt");

	fin >> n;

	for (int i = 0; i < n; i++) {
		long long num;
		fin >> num;
		hazak.push_back(num);
	}
	return;
}

long long megold(int n, vector<int> hazak, vector<long long> ados) {
	long long adossag = 0, sum = 0;
	for (int i = 0; i < n; i++) {
		adossag -= hazak[i];
		ados.push_back(adossag);
	}
	for (int i = 0; i < n; i++) {
		sum += abs(ados[i]);
	}

	return sum;
}


int main() {
	ofstream fout("test2out.txt");
	int n;
	vector<int> hazak;
	vector<long long> ados;
	beolvas(n, hazak);
	fout << megold(n, hazak, ados);

	return 0;
}