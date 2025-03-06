/*
Kijelentés
Adott 𝑛 programozó fizetése. Válogassuk szét őket a fizetésük alapján. Az
első csoportba azok kerülnek, akiknek a fizetésük nagyobb mint 1000 EUR, a
másodikba a megmaradtak közül azok, akiknek a fizetése nagyobb mint 700
EUR, a harmadikban maradnak a többiek (a sorrend nem fontos). Dolgozzunk
egyetlen tömbbel és végezzük el abban a szétválogatást, nem elég a kiíratásban
csoportokra osztani a számokat.
*/

#include <iostream>
#include <vector>
#include <fstream>

using namespace std;

void beolvas(int& n, vector<int>& a) {
    for (int i = 0; i < n; i++) {
        int temp;
        cin >> temp;
        a.push_back(temp);
    }
}

void kiir(const vector<int>& a) {
    for (int num : a) {
        cout << num << ' ';
    }
    cout << endl;
}

void szetvalogat(vector<int>& a, vector<int>& b, vector<int>& c, int tul) {
    for (int i = 0; i < a.size(); i++) {
        if (a[i]> tul) {
            b.push_back(a[i]);
        }
        else {
            c.push_back(a[i]);
        }
    }
}

int main() {
    int n;
    fin >> n;
    vector<int> a, b, c, d, e;

    beolvas(n, a);
    szetvalogat(a, b, c, 1000);
    szetvalogat(c, d, e, 700);

    kiir(b);
    kiir(d);
    kiir(e);

    return 0;
}