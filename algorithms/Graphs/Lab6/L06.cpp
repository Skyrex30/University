#include <iostream>
#include <vector>
#include <cmath>
#include <fstream>
#include <limits>

using namespace std;

struct City {
    int index;
    double x, y;
};

double distance(const City& a, const City& b) {
    return sqrt(pow(a.x - b.x, 2) + pow(a.y - b.y, 2));
}

vector<City> readCities(const string& filename) {
    ifstream inputFile(filename);
    vector<City> cities;
    if (!inputFile) {
        cerr << "Error: Nem lehet megnyitni a bemeneti filet." << endl;
        return cities;
    }

    int index;
    double x, y;
    while (inputFile >> index >> x >> y) {
        if (index < 0 || isnan(x) || isnan(y)) {
            cerr << "Error: Ervenytelen varos." << endl;
            cities.clear();
            return cities;
        }
        cities.push_back({ index, x, y });
    }
    return cities;
}

void writeOutput(const string& filename, double distance, const vector<int>& tour) {
    ofstream outputFile(filename);
    if (!outputFile) {
        cerr << "Error: Nem lehet megnyitni a kimeneti filet." << endl;
        return;
    }

    outputFile << "Ut hossza: " << distance << endl;
    outputFile << "Ut: ";
    for (int city : tour) {
        outputFile << city << " ";
    }
    outputFile << endl;
}

vector<int> greedyTSP(const vector<City>& cities) {
    int n = cities.size();
    vector<int> tour;
    vector<bool> visited(n, false);
    int currentCity = 0;
    tour.push_back(currentCity);
    visited[currentCity] = true;

    for (int i = 1; i < n; ++i) {
        double minDist = numeric_limits<double>::max();
        int nextCity = -1;

        for (int j = 0; j < n; ++j) {
            if (!visited[j] && distance(cities[currentCity], cities[j]) < minDist) {
                minDist = distance(cities[currentCity], cities[j]);
                nextCity = j;
            }
        }

      
        tour.push_back(nextCity);
        visited[nextCity] = true;
        currentCity = nextCity;
    }

    tour.push_back(tour[0]); 
    return tour;
}

double totalDistance(const vector<City>& cities, const vector<int>& tour) {
    double dist = 0.0;
    for (size_t i = 0; i < tour.size() - 1; ++i) {
        dist += distance(cities[tour[i]], cities[tour[i + 1]]);
    }
    return dist;
}

int main() {
    const string inputFileName = "input.txt";
    const string outputFileName = "output.txt";

    vector<City> cities = readCities(inputFileName);

    vector<int> tour = greedyTSP(cities);

    double dist = totalDistance(cities, tour);

    writeOutput(outputFileName, dist, tour);

    cout << "Ut hossza: " << dist << endl;
    cout << "Ut: ";
    for (int city : tour) {
        cout << city << " ";
    }
    cout << endl;

    return 0;
}
