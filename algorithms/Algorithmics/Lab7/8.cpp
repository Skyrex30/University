#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

bool isColumnInQuadrant(pair<int, int>& columnRow, pair<int, int>& quadrant1, pair<int, int>& quadrant2) {
    int x = columnRow.first, y = columnRow.second;
    int qx1 = quadrant1.first, qy1 = quadrant1.second;
    int qx2 = quadrant2.first, qy2 = quadrant2.second;
    return qx1 <= x && x < qx2 && qy1 <= y && y < qy2;
}

vector<pair<int, int>> generateLShape(pair<int, int>& quadrant, int rotation) {
    int x1 = quadrant.first, y1 = quadrant.second;

    if (rotation == 0) {
        return { {x1, y1}, {x1, y1 + 1}, {x1 + 1, y1} };
    }
    else if (rotation == 1) {
        return { {x1, y1 + 1}, {x1, y1 + 2}, {x1 + 1, y1 + 2} };
    }
    else if (rotation == 2) {
        return { {x1 + 1, y1}, {x1 + 2, y1}, {x1 + 2, y1 + 1} };
    }
}

void divideAndConquer(int n, pair<int, int>& columnRow, pair<int, int>& quadrant1, pair<int, int>& quadrant2) {
    if (n == 1) {
        return;
    }

    int half_n = n / 2;
    int x = columnRow.first, y = columnRow.second;

    pair<int, int> quadrants[4][2] = {
        {{0, 0}, {half_n, half_n}},
        {{0, half_n}, {half_n, n}},
        {{half_n, 0}, {n, half_n}},
        {{half_n, half_n}, {n, n}}
    };

    for (int i = 0; i < 4; ++i) {
        if (isColumnInQuadrant(columnRow, quadrants[i][0], quadrants[i][1])) {
            auto lShape = generateLShape(quadrants[i][0], i);
            for (const auto& point : lShape) {
                cout << point.first << " " << point.second << " ";
            }
            cout << endl;
        }
    }

    for (int i = 0; i < 4; ++i) {
        if (!(quadrants[i][0] == quadrant1 && quadrants[i][1] == quadrant2)) {
            divideAndConquer(half_n, columnRow, quadrants[i][0], quadrants[i][1]);
        }
    }
}

int main() {
    int n;
    cin >> n;

    pair<int, int> columnRow;
    cin >> columnRow.first >> columnRow.second;

    pair<int, int> quadrant1 = { 0, 0 };
    pair<int, int> quadrant2 = { 1 << n, 1 << n };

    divideAndConquer(1 << n, columnRow, quadrant1, quadrant2);

    return 0;
}
