/*

Rendezzük egy körmentes egyszerű irányított gráf csúcsait olyan sorrendbe, hogy minden él balról jobbra (egy korábbi csúcsból egy későbbi csúcsba) mutasson.

Bemeneti adatok

A graf.in állomány első sorában n és m értéke (1 <= n <= 1000, 1 <= m <= 499500). A következő sorokban a csomópontpárok találhatóak szóközzel elválasztva.

Kimeneti adatok

Szóközzel elválasztva a csomópontok sorszáma.
*/

#include <iostream>
#include <fstream>
#include <vector>
#include <stack>

using namespace std;

vector<vector<int>> adjList; 
vector<bool> visited;
stack<int> sorted;

void dfs(int v) {
    visited[v] = true;
    for (int u : adjList[v]) {
        if (!visited[u]) {
            dfs(u);
        }
    }
    sorted.push(v);
}

int main() {
    int n, m, u, v;
    ifstream input("graf.in");

    input >> n >> m;

    adjList.resize(n);
    visited.assign(n, false);

    for (int i = 0; i < m; i++) {
        input >> u >> v;

        adjList[u - 1].push_back(v - 1);
    }


    for (int i = 0; i < n; i++) {
        if (!visited[i]) {
            dfs(i);
        }
    }

    while (!sorted.empty()) {
        cout << sorted.top() + 1 << " ";
        sorted.pop();
    }

    return 0;
}
