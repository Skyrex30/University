//Nagy Csongor,513
#include <iostream>
#include <vector>
#include <climits>

using namespace std;

const int INF = INT_MAX;

void floyd_warshall(int n, vector<vector<int>>& dist, vector<vector<int>>& next_node) {
    for (int k = 0; k < n; ++k) {
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < n; ++j) {
                if (dist[i][k] != INF && dist[k][j] != INF && dist[i][k] + dist[k][j] < dist[i][j]) {
                    dist[i][j] = dist[i][k] + dist[k][j];
                    next_node[i][j] = next_node[i][k];
                }
            }
        }
    }
}

vector<int> find_shortest_path(int start, int end, const vector<vector<int>>& next_node) {
    vector<int> path;
    while (start != end) {
        if (next_node[start][end] == -1) {
            return {};
        }
        path.push_back(start);
        start = next_node[start][end];
    }
    path.push_back(end);
    return path;
}

int main() {
    int n, m, start, end;
    cin >> n >> m >> start >> end;

    vector<vector<int>> dist(n, vector<int>(n, INF));
    vector<vector<int>> next_node(n, vector<int>(n, -1));

    for (int i = 0; i < n; ++i) {
        dist[i][i] = 0;
    }

    for (int i = 0; i < m; ++i) {
        int u, v, w;
        cin >> u >> v >> w;
        dist[u][v] = w;
        next_node[u][v] = v;
    }

    floyd_warshall(n, dist, next_node);

    cout << "Tavmatrix:" << endl;
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            if (dist[i][j] == INF) {
                cout << "-1 ";
            }
            else {
                cout << dist[i][j] << " ";
            }
        }
        cout << endl;
    }

    auto shortest_path = find_shortest_path(start, end, next_node);
    if (shortest_path.empty()) {
        cout << "Nincs ut a ket csomopont kozott." << endl;
    }
    else {
        cout << "\nLegrovidebb ut: ";
        for (int node : shortest_path) {
            cout << node << " -> ";
        }
        cout << endl;
    }

    return 0;
}
