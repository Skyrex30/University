/*
Nagy Csongor, 513-as csoport, 2023-24_Lab6/06

Kijelentés
Az Encián motelben évekkel előre is fogadnak foglalásokat az odaérkező
𝑛 turistacsoport számára. Ismervén a napot, amellyel kezdve foglalni lehet
és amelyet 1-el számozunk, a csoportok megjelölik az első és az utolsó napot,
amelyet a motelben szeretnének tölteni. Az Encián tulajdonosa mindegyik
csoport számára szeretne egy tradicionális előadást szervezni, amire csak a motel
dísztermében kerülhet sor, ahova egyszerre csak egy turistacsoport fér be. Ezeken
az előadásokon fel fog lépni egy előadó, aki megszabja azt az 𝑛 napot, amikor
el tud jönni az előadásokra. Segítsetek a tulajdonosnak eldönteni, hogy melyik
turistacsoportnak melyik napra szervezze meg az előadást.
*/

#include <iostream>
#include <vector>
#include <algorithm>
#include <fstream>

using namespace std;

void input(int& n, vector<pair<pair<int, int>, int>>& a, vector<pair<int, int>>& b) {
    ifstream fin("test6.txt");
    fin >> n;
    for (int i = 0; i < n; i++) {
        int num1, num2;
        fin >> num1 >> num2;
        a.push_back(make_pair(make_pair(num1, num2), i)); 
    }
    for (int i = 0; i < n; i++) {
        int c;
        fin >> c;
        b.push_back(make_pair(c, i));
    }
}

void output(int n, vector<pair<int, int>> solution) {
    ofstream fout("test6out.txt");
    for (int i = 0; i < n; i++) {
        fout << solution[i].first + 1 << " " << solution[i].second + 1 << " " << endl;
    }
}

bool compare(pair<pair<int, int>, int>& a, pair<pair<int, int>, int>& b) {
    if (a.first.second != b.first.second) {
        return a.first.second < b.first.second;
    }

    return a.first.first < b.first.first;
}

int solve(int n, vector<pair<pair<int, int>, int>> groups, vector<pair<int, int>> shows, vector<pair<int, int>>& solution) {
    sort(groups.begin(), groups.end(), compare);
    sort(shows.begin(), shows.end());

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (shows[j].first >= groups[i].first.first && shows[j].first <= groups[i].first.second) {
                solution.push_back(make_pair(groups[i].second, shows[j].second));
                shows[j].first = 0;
                break;
            }
            if (shows[j].first > groups[i].first.second) {
                cout << 0 << " " << 0;
                return -1;
            }
        }
    }
    return 0;
}

int main() {
    int n;
    vector<pair<pair<int, int>, int>> groups; 
    vector<pair<int, int>> shows;

    input(n, groups, shows);
    vector<pair<int, int>> solution;

    int m = solve(n, groups, shows, solution);
    if (m == -1) return 0;

    output(n, solution);

    return 0;
}
