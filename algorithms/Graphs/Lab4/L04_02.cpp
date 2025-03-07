//Nagy Csongor,513
#include <iostream>
#include <vector>
#include <climits>

using namespace std;

struct Edge {
    int source, destination, weight;
};

void bellmanFord(vector<Edge>& edges, int numVertices, int source) {
    vector<int> distance(numVertices, INT_MAX);
    vector<int> parent(numVertices, -1);

    distance[source] = 0;

    // Relax 
    for (int i = 0; i < numVertices - 1; ++i) {
        for (int j = 0; j < edges.size(); ++j) {
            int u = edges[j].source;
            int v = edges[j].destination;
            int weight = edges[j].weight;
            if (distance[u] != INT_MAX && distance[u] + weight < distance[v]) {
                distance[v] = distance[u] + weight;
                parent[v] = u;
            }
        }
    }

    // megnezzuk ha van negativ kor
    for (int i = 0; i < edges.size(); ++i) {
        int u = edges[i].source;
        int v = edges[i].destination;
        int weight = edges[i].weight;
        if (distance[u] != INT_MAX && distance[u] + weight < distance[v]) { //mivel elvegeztuk a relaxot mar, ilyen nem kellene megtortenjen, es ez az jelenti hogy folyamatosan csokken, tehat negativ kor
            cout << "Van negativ kor" << endl;
            return;
        }
    }

    for (int i = 0; i < numVertices; ++i) {
        if (i != source) {
            cout << "A legrovidebb ut hossza " << i << "-ba: ";
            if (distance[i] != INT_MAX) {
                cout << distance[i] << endl;
                cout << "A legrovidebb ut " << i << "-ba: ";
                int current = i;
                vector<int> path;
                while (current != source) {
                    path.push_back(current);
                    current = parent[current];
                }
                path.push_back(source); 
                for (int j = path.size() - 1; j >= 0; --j) {
                    cout << path[j] << " "; 
                }
                cout << endl;
            }
            else {
                cout << "nem elerheto" << endl;
            }
        }
    }
}

int main() {
    int numVertices, numEdges, source;
    cin >> numVertices >> numEdges >> source;

    vector<Edge> edges(numEdges);
    for (int i = 0; i < numEdges; ++i) {
        int x, y, z;
        cin >> x >> y >> z;
        edges[i] = { x, y, z };
    }

    bellmanFord(edges, numVertices, source);

    return 0;
}
