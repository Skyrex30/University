//Nagy Csongor, 513, 4 pontos valtozat
//graf.in-bol olvas

#include <iostream>
#include <fstream>
#include <vector>
#include <queue>
using namespace std;


bool bfs(vector<vector<int>> rGraph, int s, int t, vector<int> &parent) {
	vector<bool> visited(rGraph.size(), false);

	queue<int> queue;
	queue.push(s);
	visited[s] = true;
	parent[s] = -1;

	while (!queue.empty()) {
		int u = queue.front();
		queue.pop();
		for (int v = 0; v < rGraph.size(); v++) {
			if (!visited[v] && rGraph[u][v] > 0) {
				if (v == t) {
					parent[v] = u;
					return true;
				}
				queue.push(v);
				parent[v] = u;
				visited[v] = true;
			}
		}
	}
	return false;
}

//S-tol t-ig visszateriti a maximalis folyamot
int fordFulkerson(vector<vector<int>> graph, vector<vector<int>> &rGraph, int s, int t) {
	int u, v;

	for (u = 0; u < graph.size(); u++) {
		for (v = 0; v < graph.size(); v++) {
			rGraph[u][v] = graph[u][v];
		}
	}
	vector<int> parent(graph.size());

	int max_flow = 0;

	while (bfs(rGraph, s, t, parent)) {
		int path_flow = INT_MAX;
		for (v = t; v != s; v = parent[v]) {
			u = parent[v];
			path_flow = min(path_flow, rGraph[u][v]);
		}
		for (v = t; v != s; v = parent[v]) {
			u = parent[v];
			rGraph[u][v] -= path_flow;
			rGraph[v][u] += path_flow;
		}

		max_flow += path_flow;

	}

	return max_flow;

}

void findMinCut(vector<vector<int>>& residualGraph, int source, vector<bool>& visited) {
    int V = residualGraph.size();
    queue<int> q;
    q.push(source);
    visited[source] = true;

    while (!q.empty()) {
        int u = q.front();
        q.pop();

        for (int v = 0; v < V; v++) {
            if (residualGraph[u][v] > 0 && !visited[v]) {
                q.push(v);
                visited[v] = true;
            }
        }
    }
}


int main() {
	vector<vector<int>> graph;

	ifstream infile("graf.in");
	int n, m;
	int s, t;

	infile >> n >> m;
	infile >> s >> t;
	graph.resize(n);

	for (int i = 0; i < graph.size(); i++) {
		graph[i].resize(n);
	}
	vector<vector<int>> rGraph(graph.size(), vector<int>(graph.size()));
	vector<bool> visited(n, false);


	for (int i = 0; i < m; ++i) {
		int u, v, capacity;
		infile >> u >> v >> capacity;
		graph[u][v] = capacity;
	}
	infile.close();

	ofstream outfile("graf.out");
	cout << "Max flow: ";
	cout << fordFulkerson(graph, rGraph, s, t);
	cout << endl;

	findMinCut(rGraph, s, visited);

	cout << "Min Cut: ";
	for (int i = 0; i < n; ++i) {
		if (visited[i]) {
			cout << i << " ";
		}
	}
	cout << endl;

	return 0;
}