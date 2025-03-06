/*

Adott egy összefüggő irányítatlan egyszerű gráf, határozzuk meg az 1-es csúcsból indulva a mélységi és a szélességi bejárás sorrendjét.

Bemeneti adatok:

A graf.in állomány első sorában n és m értéke (1 <= n <= 1000, 1 <= m <= 499500). A következő sorokban a csomópontpárok találhatóak szóközzel elválasztva.

Kimeneti adatok:

Az első sorba "Melysegi bejaras: ", majd a bejárt csúcsok kerüljenek szóközzel elválasztva.

A második sorba  "Szelessegi bejaras: ", majd a bejárt csúcsok kerüljenek szóközzel elválasztva.

*/
#include <iostream>
#include <fstream>
#include <vector>
#include <stack>
#include <queue>

using namespace std;

vector<vector<int>> graph;

vector<int> depthFirstSearch(int start) {
    vector<int> dfsOrder;
    vector<bool> visited(graph.size(), false);
    stack<int> stack;

    stack.push(start);

    while (!stack.empty()) {
        int vertex = stack.top();
        stack.pop();

        if (!visited[vertex]) {
            visited[vertex] = true;
            dfsOrder.push_back(vertex);

            for (int neighbor : graph[vertex]) {
                if (!visited[neighbor]) {
                    stack.push(neighbor);
                }
            }
        }
    }

    return dfsOrder;
}

vector<int> breadthFirstSearch(int start) {
    vector<int> bfsOrder;
    vector<bool> visited(graph.size(), false);
    queue<int> queue;

    queue.push(start);

    while (!queue.empty()) {
        int vertex = queue.front();
        queue.pop();

        if (!visited[vertex]) {
            visited[vertex] = true;
            bfsOrder.push_back(vertex);

            for (int neighbor : graph[vertex]) {
                if (!visited[neighbor]) {
                    queue.push(neighbor);
                }
            }
        }
    }

    return bfsOrder;
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
        graph[v].push_back(u);
    }
    infile.close();

    vector<int> dfsResult = depthFirstSearch(1);
    vector<int> bfsResult = breadthFirstSearch(1);

    ofstream outfile("graf.out");
    outfile << "Melysegi bejaras: ";
    for (int i : dfsResult) {
        outfile << i << " ";
    }
    outfile << endl;

    outfile << "Szelessegi bejaras: ";
    for (int i : bfsResult) {
        outfile << i << " ";
    }
    outfile << endl;
    outfile.close();

    return 0;
}
