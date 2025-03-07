//Nagy Csongor,513
#include <iostream>
#include <vector>
#include <algorithm>
#include <fstream>

using namespace std;

const int INF = INT_MAX;

struct Edge {
    int u, v, weight;

    Edge(int _u, int _v, int _weight) {
        u = _u;
        v = _v;
        weight = _weight;
    }

    bool operator<(const Edge& other) {
        return weight < other.weight;
    }
};

struct unionfind {
    vector<int> parent, rank;

    unionfind(int n) {
        parent.resize(n);
        rank.assign(n, 0);
        for (int i = 0; i < n; ++i)
            parent[i] = i;
    }

    int find(int u) {
        return (parent[u] == u) ? u : (parent[u] = find(parent[u]));
    }

    void unite(int u, int v) {
        int pu = find(u);
        int pv = find(v);
        if (pu != pv) {
            if (rank[pu] < rank[pv]) {
                parent[pu] = pv;
            }
            else if (rank[pu] > rank[pv]) {
                parent[pv] = pu;
            }
            else { //ha a ketto egyenlo
                rank[pu]++;
                parent[pv] = pu;
            }
        }
    }
};

int kruskal(vector<Edge>& edges, int n) {

    sort(edges.begin(), edges.end());

    unionfind uf(n);
    int totalCost = 0;
    vector<Edge> result; // A minimalis feszitofa elei

    for (Edge& e : edges) {
        if (uf.find(e.u) != uf.find(e.v)) {
            totalCost += e.weight;
            result.push_back(e);
            uf.unite(e.u, e.v);
        }
    }

    cout << "Minimalis feszitofa elei:" << endl;
    for (Edge& e : result) {
        cout << e.u << " - " << e.v << endl;
    }

    return totalCost;
}

int main() {
    ifstream fin("graf.in");
    int n, m;
    fin >> n >> m;

    vector<Edge> edges; 

    for (int i = 0; i < m; ++i) {
        int u, v, weight;
        fin >> u >> v >> weight;
        Edge e(u, v, weight);
        edges.push_back(e);
    }

    fin.close();

    int minCost = kruskal(edges, n);

    cout << "Minimalis feszitofa koltsege: " << minCost << endl;

    return 0;
}
