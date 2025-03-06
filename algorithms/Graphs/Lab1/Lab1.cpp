/*
Adott egy egyszerű gráf a graf.in állományban, a következő alakban: a graf.in állomány első sorában a csúcsok n és az élek m száma található (1 <= n <= 1000, 1 <= m <= 1 000 000). A következő m sor mindegyikében három szám szerepel: egy adott él végpontjai és az él súlya.

    Építsük fel a gráf szomszédsági mátrixát!
    Írjunk alprogramokat melyek a következő átalakító műveletet végzik el (ebben a sorrendben) (az eredményeket jelenítsük is meg):
    szomszédsági mátrix → incidencia mátrix → szomszédsági lista → élek listája → szomszédsági mátrix
*/


#include <iostream>
#include <vector>
#include <tuple>
#include <fstream>

using namespace std;

ifstream f("graf.in");

void in(int& n, int& m, vector<vector<int>>& a) {
    f >> n >> m;
    a.resize(n, vector<int>(n, 0));
    for (int i = 0; i < m; i++) {
        int kezdocsucs, vegcsucs, suly;
        f >> kezdocsucs >> vegcsucs >> suly;
        a[kezdocsucs - 1][vegcsucs - 1] = suly;
        a[vegcsucs - 1][kezdocsucs - 1] = suly;
    }
}

void out(const vector<vector<int>>& a) {
    for (int i = 0; i < a.size(); i++) {
        for (int j = 0; j < a[i].size(); j++) {
            cout << a[i][j] << " ";
        }
        cout << "\n";
    }
    cout << "\n";
}

void incidencia(int n, int m, vector<vector<int>> a, vector<vector<int>>& incidenciam) {
    int e = 0; 
    
    incidenciam.resize(n); 
    for (int i = 0; i < n; ++i) {
        incidenciam[i].resize(m); 
    }

    int elIndex = 0;
    for (int i = 0; i < n; ++i) {
        for (int j = i + 1; j < n; ++j) {
            if (a[i][j] > 0) {
                incidenciam[i][elIndex] = a[i][j]; 
                incidenciam[j][elIndex] = a[i][j]; 
                elIndex++; 
            }
        }
    }

}


void szmszdLista(const vector<vector<int>>& incidenciam, vector<vector<pair<int, int>>>& szmszdListaM) {
    int n = incidenciam.size();
    if (n == 0) return;
    int m = incidenciam[0].size();

    szmszdListaM.resize(n);

    for (int el = 0; el < m; ++el) {
        vector<int> csucsok;
        for (int csucs = 0; csucs < n; ++csucs) {
            if (incidenciam[csucs][el] != 0) {
                csucsok.push_back(csucs);
            }
        }
        if (csucsok.size() == 2) {
            szmszdListaM[csucsok[0]].push_back(make_pair(csucsok[1], incidenciam[csucsok[1]][el]));
            szmszdListaM[csucsok[1]].push_back(make_pair(csucsok[0], incidenciam[csucsok[0]][el]));
        }
    }
}

void kiirszmszdListaM(const vector<vector<pair<int, int>>>& szmszdListaM) {
    for (int i = 0; i < szmszdListaM.size(); ++i) {
        cout << i + 1 << ":";
        for (int csucs = 0; csucs < szmszdListaM[i].size(); csucs++) {
            cout << " " << szmszdListaM[i][csucs].first + 1 <<" - " << szmszdListaM[i][csucs].second <<",";
        }
        cout << "\n";
    }
    cout << "\n";
}

void ellista(int n, int m, vector<vector<pair<int, int>>> a, vector<tuple<int, int, int>> &ellistaM) {
    for (int i = 0; i < a.size(); ++i) {
        for (int j = 0; j < a[i].size(); j++) {
            if (a[i][j].first > i) {
                ellistaM.push_back(make_tuple(i, a[i][j].first, a[i][j].second));
            }
        }
    }
}

void kiirEllista(const vector<tuple<int, int, int>> a) {
    for (int i = 0; i < a.size(); i++) {
        cout << get<0>(a[i]) + 1 << " " << get<1>(a[i]) + 1 <<"-" << get<2>(a[i]) << "\n";
    }
    cout << "\n";
}

void szmszMatrix(int n, vector<tuple<int, int, int>> a, vector<vector<int>> &szmM) {
    szmM.resize(n, vector<int>(n));

    for (int i = 0; i < a.size(); i++) {
        szmM[get<0>(a[i])][get<1>(a[i])] = get<2>(a[i]);
        szmM[get<1>(a[i])][get<0>(a[i])] = get<2>(a[i]);
    }
}



int main() {
    int n, m;
    vector<vector<int>> szomszed, incidenciam, szmM;
    vector<vector<pair<int, int>>> szmszdListaM;
    vector<tuple<int, int, int>> ellistaM;

    in(n, m, szomszed);
    cout << "Szomszedsagi matrix:\n";
    out(szomszed);

    incidencia(n, m, szomszed, incidenciam);
    cout << "Incidencia matrix:\n";
    out(incidenciam);

    szmszdLista(incidenciam, szmszdListaM);
    cout << "Szomszedsagi lista:" << "\n";
    kiirszmszdListaM(szmszdListaM);

    ellista(n, m, szmszdListaM, ellistaM);
    cout << "Elek listaja:" << "\n";
    kiirEllista(ellistaM);

    szmszMatrix(n, ellistaM, szmM);
    cout << "Szomszedsagi matrix:\n";
    out(szmM);

    return 0;
}
