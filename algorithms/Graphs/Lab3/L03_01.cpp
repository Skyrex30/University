//Nagy Csongor,513
#include <iostream>
#include <vector>
#include <queue>
#include <utility>
#include <fstream>

using namespace std;

const int INF = INT_MAX; 

typedef pair<int, int> WeightedEdge;

int prim(vector<vector<WeightedEdge>>& graph, int n) {
    vector<int> dist(n, INF);
    vector<bool> visited(n, false); 
    priority_queue<WeightedEdge, vector<WeightedEdge>, greater<WeightedEdge>> pq; 

    int totalCost = 0;

    pq.push({ 0, 0 });
    dist[0] = 0;

    while (!pq.empty()) {
        int u = pq.top().second;
        int w = pq.top().first;
        pq.pop();

        if (visited[u]) continue;
        visited[u] = true;
        totalCost += w;

        for (int j = 0; j < graph[u].size(); ++j) {
            int v = graph[u][j].first;
            int weight = graph[u][j].second;

            if (!visited[v] && weight < dist[v]) {
                dist[v] = weight;
                pq.push({ dist[v], v });
            }
        }
    }

    return totalCost;
}

int main() {
    ifstream fin("graf.in");
    int n, m;
    fin >> n >> m;

    vector<vector<WeightedEdge>> graph(n);

    for (int i = 0; i < m; ++i) {
        int u, v, w;
        fin >> u >> v >> w;
        graph[u].push_back({ v, w });
        graph[v].push_back({ u, w });
    }

    fin.close();

    int minCost = prim(graph, n);

    cout << "Minimalis feszitofa koltsege: " << minCost << endl;

    return 0;
}
