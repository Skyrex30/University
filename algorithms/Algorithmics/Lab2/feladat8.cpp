/*
Kijelentés
Vizsgáljuk meg, hogy egy adott, 𝑛×𝑛 méretű négyzetes tömb bűvös négyzet-e
vagy sem. Egy természetes számokkal feltöltött négyzetes tömböt akkor nevezünk
bűvös négyzetnek, ha páronként különböző számokat tartalmaz, a számok összege
soronként és oszloponként, valamint a két átló mentén azonos. Alkalmazzuk a
Halmaz-e programozási tételt!
*/

#include <iostream>
#include <vector>

using namespace std;

void beolvas(vector<vector<uint64_t>> &matrix, int n) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            cin >> matrix[i][j];
        }
    }
}

// Halmaz-e tétel:
bool isSet(vector<uint64_t> a) {

    int i = 0, j = 0;
    int n = a.size();
    bool ok = true;
    while (ok && i < n) {
        j = i + 1;
        while (j < n && a[i] != a[j])
        {
            j++;
        }
        ok = (j >= n);
        i++;
    }

    return ok;
}


int isMagicSquare(vector<vector<uint64_t>> matrix, int n) {
    uint64_t sum1 = 0, sum2 = 0;
    uint64_t diagonalSum1 = 0, diagonalSum2 = 0;
    
    for (int i = 0; i < n; i++) {
        sum1 = 0;
        sum2 = 0;
        for (int j = 0; j < n; j++) {
            sum1 += matrix[i][j]; 
            sum2 += matrix[j][i]; 
        }
        diagonalSum1 += matrix[i][i]; 
        diagonalSum2 += matrix[i][n - i - 1]; 

        if (sum1 != sum2) {
            cout << "nem" << endl;
            return 0;
        }
    }

    if (sum1 == sum2  && sum2 == diagonalSum1 && diagonalSum1== diagonalSum2) {
        cout << "igen" << endl;
        return 0;
    }
 
    cout << "nem" << endl;
    return 0;
}

vector<uint64_t> flattenMatrix(vector<vector<uint64_t>> matrix, int n) {
    vector<uint64_t> vector;

    for (int i = 0; i < n; i++) {
        vector.insert(vector.end(), matrix[i].begin(), matrix[i].end());
    }
    return vector;
}

int main() {
    int n;
    cin >> n;

    vector<vector<uint64_t>> matrix(n, vector<uint64_t>(n));

    beolvas(matrix, n);
    vector<uint64_t> vector = flattenMatrix(matrix, n);
    if (isSet(vector) == false) {
        cout << "nem";
        return 0;
    }
    isMagicSquare(matrix, n);

    return 0;
}
