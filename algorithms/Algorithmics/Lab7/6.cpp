/* Nagy Csongor, 513-s csoport
* Lab 7/06
Kijelentés
Adott egy 𝑛 elemű tömb, mely 32 bites előjeles egész számokat tartalmaz
és egy 𝑥 32 bites előjeles egész szám. Határozzuk meg, hogy létezik-e két olyan
eleme a tömbnek, melyek összege pontosan 𝑥. Alkalmazzunk bináris keresést!
*/

#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int64_t binker(int64_t i, int64_t keresendo, vector<pair<int64_t, int64_t>>& a, int64_t b, int64_t j) {
    if (b > j) {
        return -1;
    }
    else {
        int64_t kozep = (b + j) / 2;

        if (keresendo > a[kozep].first) {
            return binker(i, keresendo, a, kozep + 1, j);
        }
        else if (keresendo < a[kozep].first) {
            return binker(i, keresendo, a, b, kozep - 1);
        }
      
        else {
            int64_t lowestIndex = -1;
            if (a[kozep].second != i) {
                lowestIndex = kozep;
                while (lowestIndex > 0 && a[lowestIndex - 1].second != i && a[lowestIndex - 1].first == a[lowestIndex].first) {
                    lowestIndex--;
                }
                lowestIndex = a[lowestIndex].second;
            }
            else {
                return binker(i, keresendo, a, kozep + 1, j);               // ha a[kozep].second = i (aminek keressük a párját) akkor megkell nézni ha esetleg fennebb van e még keresett szám, mert lehet hogy az első pont i lesz.
            }
            return lowestIndex;
        }
    }
}

bool comparePairs(const pair<int64_t, int64_t>& a, const pair<int64_t, int64_t>& b) {
    if (a.first == b.first) {
        return a.second < b.second;
    }
    return a.first < b.first;
}

void megold(int64_t n, int64_t x, vector<int64_t>& a) {
    vector<pair<int64_t, int64_t>> b;
    for (int64_t i = 0; i < n; i++) {
        b.push_back(make_pair(a[i], i));                // a masodik ertek lesz az eredeti tomb indexe
    }

    sort(b.begin(), b.end(), comparePairs);

    for (int64_t i = 0; i < n; i++) {
        int64_t keresendo = x - a[i];
        int64_t j = binker(i, keresendo, b, 0, b.size() - 1);
        if (j != -1) {
            cout << 1 << endl;
            cout << i + 1 << " " << j + 1;
            return;
        }
    }
    cout << 0;
    return;
}

void input(int64_t& n, int64_t& x, vector<int64_t>& a) {
    cin >> n >> x;

    for (int64_t i = 0; i < n; i++) {
        int64_t num;
        cin >> num;
        a.push_back(num);
    }

    return;
}


int main() {
    int64_t n, x;
    vector<int64_t> a;

    input(n, x, a);

    megold(n, x, a);

    return 0;
}
