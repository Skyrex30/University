//Nagy Csongor,513
#include <iostream>
#include <vector>
#include <queue>
#include <climits>

using namespace std;

typedef pair<int, int> WeightedEdge; 
typedef vector<vector<WeightedEdge>> Graph;
const int INF = INT_MAX;

void dijkstra(const Graph& graph, int start, vector<int>& distance, vector<int>& parent) {
    int n = graph.size();
    vector<bool> visited(n, false);
    distance.assign(n, INF);
    parent.assign(n, -1);

    distance[start] = 0;
    priority_queue<WeightedEdge, vector<WeightedEdge>, greater<WeightedEdge>> pq; 

        pq.push({ 0, start });

    while (!pq.empty()) {
        int u = pq.top().second;
        pq.pop();

        if (visited[u]) continue;
        visited[u] = true;

        for (int i = 0; i < graph[u].size(); ++i) {
            int v = graph[u][i].first;
            int weight = graph[u][i].second;
            if (!visited[v] && distance[u] + weight < distance[v]) {
                distance[v] = distance[u] + weight;
                parent[v] = u;
                pq.push({ distance[v], v });
            }
        }
    }
}

void shortest_paths(const Graph& graph, int start) {
    int n = graph.size();
    vector<int> distance, parent;
    dijkstra(graph, start, distance, parent);

    for (int u = 0; u < n; ++u) {
        if (u != start) {
            vector<int> path;
            if (distance[u] != INF) {
                int v = u;
                while (v != -1) {
                    path.push_back(v);
                    v = parent[v];
                }
                reverse(path.begin(), path.end());
                cout << "A legrovidebb ut hossza " << u << "-ba: " << distance[u] << endl;
                cout << "A legrovidebb ut " << u << "-ba: ";
                for (int i = 0; i < path.size(); ++i) {
                    cout << path[i] << (i == path.size() - 1 ? "\n" : " ");
                }
            }
            else {
                cout << "A legrovidebb ut hossza " << u << "-ba: nem elerheto" << endl;
                cout << "A legrovidebb ut " << u << "-ba: nem elerheto" << endl;
            }
        }
    }
}

int main() {
    int n, m, start;
    cin >> n >> m >> start;

    Graph graph(n);

    for (int i = 0; i < m; ++i) {
        int x, y, z;
        cin >> x >> y >> z;
        graph[x].push_back({ y, z });
    }

    shortest_paths(graph, start);

    return 0;
}
