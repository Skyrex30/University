/*
Adott egy összefüggő irányított egyszerű gráf. Szélességi bejárás segítségével határozzuk meg egy adott csomópont k-ad rendű "ismerősét". k-ad rendű ismerős alatt azt értjük, hogy az adott csomópontból azokba a csomópontokba k hosszúságú a legrövidebb út.

Bemeneti adatok

A graf.in állomány első sorában n és m értéke (1 <= n <= 1000, 1 <= m <= 499500). A második sorban az adott csomópont és a k értéke (szóközzel elválasztva). A következő sorokban a csomópontpárok találhatóak szóközzel elválasztva.

Kimeneti adatok

A k-ad rendű ismerősök szóközzel elválasztva.
*/

#include <iostream>
#include <fstream>
#include <vector>
#include <queue>
#include <unordered_set>
using namespace std;

vector<vector<int>> graph;

vector<int> kthNeighbors(int start, int k, const vector<vector<int>>& graph) {
    vector<int> result;
    queue<int> q;
    unordered_set<int> visited;

    q.push(start);
    visited.insert(start);

    int currentLevel = 0;

    while (!q.empty() && currentLevel <= k) {
        int size = q.size();

        for (int i = 0; i < size; ++i) {
            int node = q.front();
            q.pop();

            if (currentLevel == k) {
                result.push_back(node);
            }

            for (int neighbor : graph[node]) {
                if (visited.find(neighbor) == visited.end()) {
                    q.push(neighbor);
                    visited.insert(neighbor);
                }
            }
        }

        ++currentLevel;
    }

    return result;
}

int main() {
    ifstream infile("graf.in");
    int n, m, startNode, k;
    infile >> n >> m >> startNode >> k;

    graph.resize(n + 1);

    for (int i = 0; i < m; ++i) {
        int u, v;
        infile >> u >> v;
        graph[u].push_back(v);
    }
    infile.close();
    
    vector<int> bfsResult = kthNeighbors(startNode, k, graph);

    ofstream outfile("graf.out");
    
    outfile << "K-adik szomszed: ";
    for (int i = 0; i < bfsResult.size(); i++) {
            outfile << bfsResult[i] << " ";
    }
    outfile << endl;
    outfile.close();

    return 0;
}
