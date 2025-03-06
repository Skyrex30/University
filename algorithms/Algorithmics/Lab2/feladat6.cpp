/*
Kijelent�s
Ismerj�k k�t sz�m pr�msz�moszt�it, hat�rozzuk meg a k�t sz�m legkisebb
k�z�s t�bbsz�r�s�nek pr�msz�moszt�it! (A k�t sz�mot nem ismerj�k, csup�n a
pr�moszt�ikat)
*/

#include <iostream>
#include <vector>

using namespace std;

void beolvas(vector<uint64_t>& a) {
    int n;
    cin >> n;
    for (int i = 0; i < n; i++) {
        uint64_t num;
        cin >> num;
        a.push_back(num);
    }
}

void kiir(const vector<uint64_t>& a) {
    for (int i = 0; i < a.size(); i++) {
        cout << a[i] << ' ';
    }
}

void egyesit(const vector<uint64_t>& a, const vector<uint64_t>& b, vector<uint64_t>& c) {
    c = a; 

    for (int i = 0; i < b.size(); i++) {
        bool exists = false;
        for (int j = 0; j < c.size(); j++) {
            if (b[i] == c[j]) {
                exists = true;
                break;
            }
        }
        if (!exists) {
            c.push_back(b[i]);
        }
    }
}

int main() {
    vector<uint64_t> a, b, c;
    beolvas(a);
    beolvas(b);
    egyesit(a, b, c);

    kiir(c);

    return 0;
}

