/*

Adott egy összefüggő irányított egyszerű gráf. Állapítsuk meg, hogy a gráf tartalmaz-e kört!

Bemeneti adatok

A graf.in állomány első sorában n és m értéke (1 <= n <= 1000, 1 <= m <= 499500). A következő sorokban a csomópontpárok találhatóak szóközzel elválasztva.

Kimeneti adatok

Egyetlen szó (igen vagy nem).
*/

#include <iostream>
#include <vector>
#include <fstream>
#include <unordered_set>

using namespace std;

vector<vector<int>> graph;

bool hasCycleUtil(int node, vector<bool>& visited, vector<bool>& recStack) {
    if (!visited[node]) {
        visited[node] = true;
        recStack[node] = true;

        for (int neighbor : graph[node]) {
            if (!visited[neighbor] && hasCycleUtil(neighbor, visited, recStack)) {
                return true;
            }
            else if (recStack[neighbor]) {
                return true;
            }
        }
    }
    recStack[node] = false;
    return false;
}

bool hasCycle(int n) {
    vector<bool> visited(n + 1, false);
    vector<bool> recStack(n + 1, false);

    for (int i = 1; i <= n; ++i) {
        if (hasCycleUtil(i, visited, recStack)) {
            return true;
        }
    }
    return false;
}

int main() {
    ifstream infile("graf.in");
    int n, m;
    infile >> n >> m;

    graph.resize(n + 1);


    for (int i = 0; i < m; ++i) {
        int u, v;
        infile >> u >> v;
        graph[u].push_back(v);
    }
    infile.close();

    bool hasCycleResult = hasCycle(n);

    ofstream outfile("graf.out");
    if (hasCycleResult) {
        outfile << "igen" << endl;
    }
    else {
        outfile << "nem" << endl;
    }
    outfile.close();

    return 0;
}
