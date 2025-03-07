//Nagy Csongor,513
#include <iostream>
#include <vector>
#include <stack>
#include <algorithm>
#include <fstream>

using namespace std;

class StronglyConnectedComponents {
private:
    int n; 
    vector<vector<int>> graph; 
    vector<int> ids;
    vector<int> low;
    vector<bool> onStack; 
    stack<int> st; 
    int idCounter; 

    // Tarjan
    void dfs(int u, vector<vector<int>>& sccs) {
        st.push(u);
        onStack[u] = true;
        ids[u] = low[u] = idCounter++;

        for (int j = 0; j < graph[u].size(); ++j) { 
            int v = graph[u][j];
            if (ids[v] == -1) {
                dfs(v, sccs);
                low[u] = min(low[u], low[v]);
            }
            else if (onStack[v]) {
                low[u] = min(low[u], ids[v]);
            }
        }

        if (ids[u] == low[u]) { 
            vector<int> scc;
            int v;
            do {
                v = st.top();
                st.pop();
                onStack[v] = false;
                scc.push_back(v);
            } while (v != u);
            sccs.push_back(scc);
        }
    }

public:
    StronglyConnectedComponents(int _n) : n(_n), idCounter(0) {
        graph.resize(n);
        ids.assign(n, -1);
        low.resize(n);
        onStack.assign(n, false);
    }

    void addEdge(int u, int v) {
        graph[u].push_back(v);
    }

    vector<vector<int>> getStronglyConnectedComponents() {
        vector<vector<int>> sccs; 
        for (int i = 0; i < n; ++i) {
            if (ids[i] == -1) {
                dfs(i, sccs);
            }
        }
        return sccs;
    }
};

int main() {
    ifstream fin("graf.in");
    int n, m;
    fin >> n >> m;

    StronglyConnectedComponents scc(n);

    for (int i = 0; i < m; ++i) {
        int u, v;
        fin >> u >> v;
        scc.addEdge(u, v);
    }

    fin.close();

    vector<vector<int>> sccs = scc.getStronglyConnectedComponents();

    ofstream fout("scc.out");
    for (const auto& scc : sccs) {
        for (int node : scc) {
            fout << node << " ";
        }
        fout << endl;
    }
    fout.close();

    return 0;
}
