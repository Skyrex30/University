//Nagy Csongor,513
#include <iostream>
#include <vector>
#include <queue>
#include <climits>
#include <stack>
using namespace std;

struct Edge {
    int src, dest, weight;
};

void johnson(vector<vector<int>>& graph, vector<Edge>& edges, int N, int source) {
    vector<int> h(N + 1, 0);
    vector<int> dist(N, INT_MAX);

    // Bellman-Ford
    for (int i = 0; i < N - 1; ++i) {
        for (auto edge : edges) {
            int u = edge.src;
            int v = edge.dest;
            int weight = edge.weight;
            if (dist[u] != INT_MAX && dist[u] + weight < dist[v]) {
                dist[v] = dist[u] + weight;
            }
        }
    }

    for (int i = 0; i < N; ++i) {
        h[i] = dist[i];
    }

    // Dijkstra
    for (int u = 0; u < N; ++u) {
        vector<int> dist(N, INT_MAX);
        dist[u] = 0;

        priority_queue<pair<int, int>, vector<pair<int, int>>, greater<pair<int, int>>> pq;
        pq.push({ 0, u });

        while (!pq.empty()) {
            int du = pq.top().first;
            int node = pq.top().second;
            pq.pop();

            if (du > dist[node]) continue;

            for (int i = 0; i < N; ++i) {
                if (graph[node][i] != INT_MAX) {
                    int v = i;
                    int weight = graph[node][i] + h[node] - h[v];
                    if (dist[node] + weight < dist[v]) {
                        dist[v] = dist[node] + weight;
                        pq.push({ dist[v], v });
                    }
                }
            }
        }

        cout << u << ". csomopont:" << endl;
        for (int v = 0; v < N; ++v) {
            if (u != v) {
                if (dist[v] == INT_MAX) {
                    cout << "A legrovidebb ut hossza " << v << "-be: nem elerheto" << endl;
                }
                else {
                    cout << "A legrovidebb ut hossza " << v << "-be: " << dist[v] + h[v] - h[u] << endl;
                    cout << "A legrovidebb ut " << u << "-bol " << v << "-be: ";
                    int current = v;
                    stack<int> path;
                    while (current != u) {
                        path.push(current);
                        int prev = current;
                        for (int i = 0; i < N; ++i) {
                            if (graph[i][current] != INT_MAX && dist[current] == dist[i] + graph[i][current] + h[current] - h[i]) {
                                current = i;
                                break;
                            }
                        }
                        if (prev == current) break;
                    }
                    path.push(u);
                    while (!path.empty()) {
                        cout << path.top() << " ";
                        path.pop();
                    }
                    cout << endl;
                }
            }
        }
        cout << endl;
    }
}


int main() {
    int N, M, source;
    cin >> N >> M >> source;

    vector<vector<int>> graph(N, vector<int>(N, INT_MAX));
    vector<Edge> edges(M);

    for (int i = 0; i < M; ++i) {
        int u, v, weight;
        cin >> u >> v >> weight;
        graph[u][v] = weight;
        edges[i] = { u, v, weight };
    }

    johnson(graph, edges, N, source);

    return 0;
}
